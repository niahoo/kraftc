-module(kl_codegen).

-include_lib("kraft/include/kraft_lang.hrl").
-compile({parse_transform, cut}).

-export([build_forms/1,deep_literal/1]).

build_forms(#kraftmod{signatures=undefined}) ->
    { error,
      io_lib:format("Signatures are required to build forms",[]) };
build_forms(#kraftmod{signatures=Signatures,parsetree=ParseTree}=KraftMod) ->
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
                           ,  TechnicInfos
                           ,  TechnicsBodies
                            ])

                          ),
    {ok,KraftMod#kraftmod{forms=Module}}.

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

ccall(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    LitM = cerl:c_atom(M),
    LitF = cerl:c_atom(F),
    cerl:c_call(LitM, LitF, A).

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

% compile_technics(TechnicDefsGs) -> {[],[]};
compile_technics(TechnicDefsGs) ->
    Exports = [cerl:c_fname(Name,Arity) || {{Name,Arity},_} <- TechnicDefsGs],
    kl:log("Exports ~p",[Exports]),
    Bodies = compile_technics_bodies(TechnicDefsGs),
    {Exports,Bodies}.

compile_technics_bodies([]) -> [];
compile_technics_bodies([{{_Name,_Arity},_TDs}=TDGroup|TechnicDefs]) ->
    kl:log("TDGroup ~n~p",[TDGroup]),
    Body = comptbody(TDGroup),
    [Body|compile_technics_bodies(TechnicDefs)].


%%% ------------------------------------------------------------------
%%% COMPILATION FUN BODY
%%% ------------------------------------------------------------------

%% Compilation du body de la technic. Les différentes clauses de la
%% fonction sont en fait des 'case'. Il faut donc créer une case
%% clause pour chaque function clause et lui donner le body qy'il faut

comptbody({{Name,Arity},TDs}) ->
    %% Définition des variables en tête de la fun Core Erlang (pas de
    %% patternmatching)
    HeadVars = make_typevar_list(Arity),
    %% Définition d'une structure 'case' à laquelle on ajoutera les
    %% clauses de chaque body. En fait, c'est la liste des variables
    %% elle-même
    BodyClauses = [comptbodyclause(Arity,TD) || TD <- TDs] ++ [function_clause_error_clause()],
    Body = cerl:c_case(cerl:make_list(HeadVars),BodyClauses),
    { cerl:c_fname(Name,Arity)
    , cerl:c_fun(HeadVars, Body)
    }.


comptbodyclause(Arity,TD) ->
    Vars = [type_expr_to_typevar(TE) || TE <- kl_technicdef:type_exprs(TD)],
    %% ON match une liste d'arguments donc il faut une liste
    Pattern = cerl:make_list(Vars),
    kl:log("Pattern ~p",[Pattern]),
    cerl:c_clause([Pattern],Pattern).


type_expr_to_typevar({_,{typename,_Line,TypeName}, 'ANON',          _Qtty}) ->
    AnonVar = cerl:c_var(cat_atoms('_Anon_',kl:unok(kl:uuid()))),
    cerl:c_tuple([cerl:c_atom(TypeName),AnonVar]);
type_expr_to_typevar({_,{typename,_,TypeName},     {name,_,Varname},_Qtty}) ->
    TypeVar = make_typevar(Varname),
    cerl:c_tuple([cerl:c_atom(TypeName),TypeVar]).

%%% ------------------------------------------------------------------
%%% HELPERS
%%% ------------------------------------------------------------------

cat_atoms(A,B) ->
    list_to_atom(kl:to_list(A)++kl:to_list(B));

cat_atoms(A,B) when is_list(A),is_list(B) ->
    list_to_atom(A++B).


make_typevar(Atom) -> cerl:c_var(cat_atoms('Var_',Atom)).

make_typevar_list(Arity)  ->
    make_typevar_list(Arity,[]).

make_typevar_list(0,    Acc) -> lists:reverse(Acc);
make_typevar_list(Arity,Acc) when Arity > -1 ->
    Var = cerl:c_var(cat_atoms('_ktArg',Arity)),
    make_typevar_list(Arity-1,[Var|Acc]).

function_clause_error_clause() ->
    Any = cerl:c_var('_Any'),
    cerl:c_clause([Any], ccall(erlang,error,[cerl:c_atom(function_clause)])).
