-module(klcheck_vardefs).
-export([check/1]).
-compile({parse_transform, do}).

check({parsetree,ParseTree}) ->
    check_undefined_vars(ParseTree).

%% Check undef vars -------------------------------------------------

check_undefined_vars([]) -> ok;
check_undefined_vars([TechnicDef|ParseTree]) ->
    check_undefined_vars2(TechnicDef).

%% On va vérifier que toutes les variables utilisées sont bien
%% déclarées au préalable
check_undefined_vars2(TechnicDef) ->
    do([error_m ||
        %% Récup des noms définis dans les types
        TypeVars <- get_type_vars(type_exprs(TechnicDef), []),
        %% Récup des noms définis dans les meta (avec comme acc de base ceux des types)
        AllDef <- get_meta_vars(meta_vars(TechnicDef), TypeVars),
        check_body(body(TechnicDef),AllDef)
    ]).


get_type_vars([], Acc) -> {ok, lists:reverse(Acc)};
get_type_vars([TypeExpr|Ts], Acc) ->
    case TypeExpr
        of {typeinput, _, 'ANON', _} -> get_type_vars(Ts,Acc)
         ; {typeinput, _, {name,_,Name}, _} -> get_type_vars(Ts,[Name|Acc])
    end.

%% Les meta peuvent être définies en fonction d'autres variables, on
%% regarde donc, lors de la définition d'une variable, si les
%% variables utilisée à droite de ':' sont définies dans le Acc
get_meta_vars([], Acc) -> {ok, lists:reverse(Acc)};
get_meta_vars([Metadef|Ms], Acc) ->
    {{name,_,Name}, Expr} = Metadef,
    VarsUsed = get_all_names(Expr),
    case ensure_all_member(VarsUsed,Acc)
        of {error,Reason} -> {error,Reason}
         ; ok -> get_meta_vars(Ms,[Name|Acc])
    end.

check_body(Body,Vars) ->
    VarsUsed = get_all_names(Body),
    ensure_all_member(VarsUsed,Vars).

%% -----------------------------------------------------------------
%% TechnicDef Accessors
%% -----------------------------------------------------------------

technicdef() -> {technicdef,name,typeexprs,metas,body}.

type_exprs({technicdef,_,TypeExprs,_,_}) -> TypeExprs.
meta_vars({technicdef,_,_,Metas,_}) -> Metas.
body({_,_,_,_,Body}) -> Body.

%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------

%% renvoie tous les names trouvés. On cherche simplement un 3-tuple
%% commençant par name. Pour simplifier, on peut aussi renvoyer nil.
%% à la fin la liste est flattened et traversée pour virer les nil
get_all_names(X) ->
    All = gan(X),
    lists:filter( fun(nil) -> false ;
                     ({X,_Line}) when is_atom(X) -> true
                  end,
                  lists:flatten(All)).

gan({var,Line,Name}) -> {Name,Line};
gan('ANON') -> nil;

gan(List) when is_list(List) ->
    lists:map(fun gan/1, List);

gan(T) when is_tuple(T) ->
    gan(tuple_to_list(T));

gan(_) -> nil.


%% Vérifie que pour tout element {X,_} de Elems, X est dans la liste List
ensure_all_member(Elems,List) ->
    %% On utilise une simple monade : si on renvoie {error,Reason} le
    %% reste des éléments n'est pas évalué, l'erreur traverse le reste
    %% de la liste
    MicroMonad =
                fun (_Elem, {error,Reason}=Lift) ->
                        Lift
                  ; ({Name,Line},  ok) when is_atom(Name), is_integer(Line)->
                        case lists:member(Name,List)
                            of true ->
                                ok
                             ; false ->
                                {error, io_lib:format("Undefined var ~p on line ~p",[Name,Line])}
                        end
                end,
    lists:foldl(MicroMonad,ok,Elems).
