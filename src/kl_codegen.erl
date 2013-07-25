-module(kl_codegen).

-include_lib("kraftc/include/kraft_lang.hrl").
-compile({parse_transform, cut}).

-export([build_forms/1,deep_literal/1]).

build_forms(#kraftmod{signatures=undefined}) ->
    { error,
      io_lib:format("Signatures are required to build forms",[]) };
build_forms(#kraftmod{signatures=Signatures,parsetree=ParseTree,filename=Filename}=KraftMod) ->
    LitModuleName = cerl:c_atom(KraftMod#kraftmod.name),
    ModuleInfoFuns = get_module_info(LitModuleName),
    TechnicInfos = get_technic_infos(Signatures),
    TechnicDefsGs = kl_parsetree:group_technicdefs(ParseTree),

    {TechnicsExports,TechnicsBodies} = compile_technics(TechnicDefsGs),
    Module = cerl:c_module( LitModuleName
                          , [ cerl:c_fname(module_info,0)
                            , cerl:c_fname(module_info,1)
                            , cerl:c_fname(technics_infos,0)
                            , cerl:c_fname(technic_infos,1)
                            ] ++ TechnicsExports
                          , lists:append([
                                ModuleInfoFuns
                              , TechnicInfos
                              , TechnicsBodies
                            ])

                          ),
    Annot = cerl:add_ann([{file,filename:basename(Filename)}],Module),
    {ok,KraftMod#kraftmod{forms=Annot}}.

get_module_info(LitName) ->
    Key = cerl:c_var('Key'),
    [ { cerl:c_fname(module_info,0)
      , cerl:c_fun([], ccall(erlang,get_module_info,[LitName]))
      }
    , { cerl:c_fname(module_info,1)
      , cerl:c_fun([Key], ccall(erlang,get_module_info,[LitName,Key]))
      }
    ].

get_technic_infos(Signatures) ->
    Key = cerl:c_var('Key'),
    [ { cerl:c_fname(technics_infos,0)
      , cerl:c_fun([], signatures_literal(Signatures))
      }
    , { cerl:c_fname(technic_infos,1)
      , cerl:c_fun([Key], signatures_literal_case(Signatures,Key))
      } ].


signatures_literal(Signatures) ->
    deep_literal(Signatures).

signatures_literal_case(Signatures,Arg) ->
    MkCaseClause =
        fun({TechnicName,TechnicClauses}) ->
            cerl:c_clause([cerl:c_atom(TechnicName)], deep_literal(TechnicClauses))
        end,
    WildCardVar = cerl:c_var('_Any'),
    WildCardClause = cerl:c_clause([WildCardVar], cerl:c_atom(undefined)),
    CaseClauses = [MkCaseClause(S) || S <- Signatures] ++ [WildCardClause],
    cerl:c_case(Arg,CaseClauses).




% compile_technics(TechnicDefsGs) -> {[],[]};
compile_technics(TechnicDefsGs) ->
    Exports = [cerl:c_fname(Name,Arity) || {{Name,Arity},_} <- TechnicDefsGs],
    % kl:log("Exports ~p",[Exports]),
    Bodies = compile_technics_functions(TechnicDefsGs),
    {Exports,Bodies}.

compile_technics_functions([]) -> [];
compile_technics_functions([{{_Name,_Arity},_TDs}=TDGroup|TechnicDefs]) ->
    % kl:log("TDGroup ~n~p",[TDGroup]),
    Function = compile_function(TDGroup),
    [Function|compile_technics_functions(TechnicDefs)].


%%% ------------------------------------------------------------------
%%% COMPILATION FUN
%%% ------------------------------------------------------------------

%% Compilation des technic. Les différentes clauses de la fonction
%% sont en fait des 'case'. Il faut donc créer une case clause pour
%% chaque function clause et lui donner le body qy'il faut

compile_function({{Name,Arity},TDs}) ->
    %% Définition des variables en tête de la fun Core Erlang (pas de
    %% patternmatching)
    HeadVars = make_var_list(Arity),
    %% Définition d'une structure 'case' à laquelle on ajoutera les
    %% clauses de chaque body. En fait, c'est la liste des variables
    %% elle-même
    FunctionClauses = [compile_functionclause(TD) || TD <- TDs] ++ [function_clause_error_clause()],
    Function = cerl:c_case(cerl:make_list(HeadVars), FunctionClauses),
    { cerl:c_fname(Name,Arity)
    , cerl:c_fun(HeadVars, Function)
    }.


compile_functionclause(TD) ->
    Vars = [type_expr_to_typevar(TE) || TE <- kl_technicdef:type_exprs(TD)],
    %% ON match une liste d'arguments donc il faut une liste
    Pattern = cerl:make_list(Vars),
    Body = compile_body(TD),
    cerl:c_clause([Pattern],Body).

compile_body(TD) ->
    TDBody = kl_technicdef:body(TD),
    cexpr(TDBody).

%%% ------------------------------------------------------------------
%%% COMPILATION EXPRESSIONS
%%% ------------------------------------------------------------------

%% Compilation des expressions imbriquées qui constituent un body. Les
%% technics n'ont pas de séquences d'expressions, c'est une expression
%% unique, par exemple un type result. Cela peut également être un
%% draw par exemple, qui branche vers différentes sous expressions.
%% Mais dans tous les cas, le body est un arbre d'expressions avec une
%% racine unique. par conséquent, compiler le body d'une technique
%% consiste à en compiler l'expression racine.

cexpr({return,TypeOutputs}) ->
    cerl:make_list(lists:map(fun cexpr/1, TypeOutputs));

cexpr({{typeoutput,{typename,_Line,TypeName},_Varname,QttyExpr},PropsExprs}) ->
    LitTypeName = cerl:c_atom(TypeName),
    Qtty = cexpr(QttyExpr),
    Props = compile_propdefs(PropsExprs),
    cerl:c_tuple([LitTypeName,Qtty,Props]);
    % cerl:c_tuple([LitTypeName,Qtty]);

cexpr({var,_,Varname}) -> make_var(Varname);
cexpr({number,_,_}=NumExpr) -> cnumber(NumExpr);
cexpr({string,_,String}) -> cerl:abstract(String);

cexpr({'+',Operand1,Operand2}) -> ccall(erlang,'+',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({'*',Operand1,Operand2}) -> ccall(erlang,'*',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({'-',Operand1,Operand2}) -> ccall(erlang,'-',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({'/',Operand1,Operand2}) -> ccall(erlang,'/',[cexpr(Operand1),cexpr(Operand2)]);

cexpr({call,'+',[Operand1,Operand2]}) -> ccall(erlang,'+',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({call,'*',[Operand1,Operand2]}) -> ccall(erlang,'*',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({call,'-',[Operand1,Operand2]}) -> ccall(erlang,'-',[cexpr(Operand1),cexpr(Operand2)]);
cexpr({call,'/',[Operand1,Operand2]}) -> ccall(erlang,'/',[cexpr(Operand1),cexpr(Operand2)]);

cexpr({call,{name,_Line,CallName},Args}) ->
    cexpr({call,CallName,Args});

cexpr({call,getprop,[{key,_,Key},Var]}) ->
    cexpr({call,getprop,[Key,Var]});

cexpr({call,CallName,Args}) ->
    % kl:log("AAAARGHS ~p",[Args]),
    LitArgs = lists:map(fun cexpr/1, Args),
    ccall(kl_lib,CallName,LitArgs);

cexpr({draw,ToMAtch,Clauses}) ->
    CaseClauses = [draw_clause(C) || C <- Clauses],
    % kl:term_to_paper(CaseClauses),
    cerl:c_case(cexpr(ToMAtch), CaseClauses);

cexpr(Atom) when is_atom(Atom) -> cerl:c_atom(Atom);

cexpr(Other) ->
    kl:term_to_paper(Other),
    deep_literal(Other).

%% ------ Propdefs ----------------------------------------------

compile_propdefs(PropsExprs) ->
    cerl:make_list(lists:map(fun compile_propdef/1, PropsExprs)).

compile_propdef({{name,_,Name},Expr}) ->
    cerl:c_tuple([cerl:c_atom(Name),cexpr(Expr)]).

%% ------ Draws ----------------------------------------------

draw_clause(none_match_clause) ->
    Any = cerl:c_var(cat_atoms('_nomatch',kl:uuid())),
    cerl:c_clause([Any], ccall(erlang, error, [cerl:c_atom(draw_clause)]));
draw_clause({{'_',_},Result}) ->
    MatchVar = make_var(cat_atoms('_otherwise',kl:uuid())),
    cerl:c_clause([MatchVar], cexpr(Result));
draw_clause({ToBeat,Result}) ->
    MatchVar = make_var(cat_atoms('Term_',kl:uuid())),
    LitToBeat = cexpr(ToBeat),
    ComparisonGuard = ccall(erlang,'>',[MatchVar,LitToBeat]),
    cerl:c_clause([MatchVar],ComparisonGuard,cexpr(Result)).
    % cerl:c_clause([MatchVar],cexpr(Result)).


%%% ------------------------------------------------------------------
%%% HELPERS
%%% ------------------------------------------------------------------

type_expr_to_typevar({_,{typename,_Line,TypeName}, 'ANON',          _Qtty}) ->
    AnonVar = cerl:c_var(cat_atoms('_Anon_',kl:uuid())),
    cerl:c_tuple([cerl:c_atom(TypeName),AnonVar]);
type_expr_to_typevar({_,{typename,_,TypeName},     {name,_,Varname},_Qtty}) ->
    TypeVar = make_var(Varname),
    cerl:c_tuple([cerl:c_atom(TypeName),TypeVar]).

cat_atoms(A,B) ->
    list_to_atom(kl:to_list(A)++kl:to_list(B)).


make_var(Atom) -> cerl:c_var(cat_atoms('Var_',Atom)).

make_var_list(Arity)  ->
    make_var_list(Arity,[]).

make_var_list(0,    Acc) -> lists:reverse(Acc);
make_var_list(Arity,Acc) when Arity > -1 ->
    Var = cerl:c_var(cat_atoms('_ktArg',Arity)),
    make_var_list(Arity-1,[Var|Acc]).

% case_clause_error_clause() ->
    % Any = cerl:c_var(cat_atoms('_otherwise',kl:uuid())),
    % cerl:c_clause([Any], ccall(erlang, error, [cerl:c_atom(case_clause)])).
function_clause_error_clause() ->
    Any = cerl:c_var(cat_atoms('_otherwise',kl:uuid())),
    cerl:c_clause([Any], ccall(erlang, error, [cerl:c_atom(function_clause)])).

%% @todo apparemment c'est inutile comme fonction deep litteral vu que
%% au final seule la structure parente se retrouve dans un tuple
%% c_literal.

deep_literal(List) when is_list(List) ->
    loglit("lit. ~w",[List]),
    cerl:make_list(lists:map(fun deep_literal/1, List));

deep_literal(Tuple) when is_tuple(Tuple) ->
    loglit("lit. ~w",[Tuple]),
    List = tuple_to_list(Tuple),
    Literals = lists:map(fun deep_literal/1, List),
    cerl:c_tuple(Literals);

deep_literal(Atom) when is_atom(Atom) ->
    loglit("lit. ~w",[Atom]),
    cerl:c_atom(Atom);

deep_literal(Float) when is_float(Float) ->
    loglit("lit. ~w",[Float]),
    cerl:c_float(Float);

deep_literal(Int) when is_integer(Int) ->
    loglit("lit. ~w",[Int]),
    cerl:c_int(Int).

loglit(_X,_Y) ->
    % kl:log(_X,_Y),
    ok.

cnumber({number,_Line,Number}) when is_integer(Number)
 -> cerl:c_int(Number);
cnumber({number,_Line,Number}) when is_float(Number)
 -> cerl:c_float(Number).


ccall(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    LitM = cerl:c_atom(M),
    LitF = cerl:c_atom(F),
    cerl:c_call(LitM, LitF, A).
