-module(kraft_checker).
-export([check/1]).
-compile({parse_transform, do}).

check(ParseTree) ->
    do([error_m ||
        _1 <- check_undefined_vars(ParseTree),
        ok
    ]).


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
        TypeAndMetaVars <- get_meta_vars(meta_vars(TechnicDef), TypeVars),
        ok
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
    {{name,_,CurrentName}, Expr} = Metadef,
    VarsUsed = get_all_names(Expr),
    MicroMonad = fun (_Elem, {error,Reason}=Lift) ->
                        Lift
                   ; ({Name,Line},  ok) when is_atom(Name), is_integer(Line)->
                        case lists:member(Name,Acc)
                            of true -> ok
                             ; false ->
                                io:format("~p not member of ~p",[Name,Acc]),
                                {error, io_lib:format("Undefined var ~p in meta expression on line ~p",[Name,Line])}
                        end
                 end,
    case lists:foldl(MicroMonad,ok,VarsUsed)
        of {error,Reason} -> {error,Reason}
         ; ok -> get_meta_vars(Ms,[CurrentName|Acc])
    end.

%% -----------------------------------------------------------------
%% TechnicDef Accessors
%% -----------------------------------------------------------------

technicdef() -> {technicdef,name,typeexprs,metas,body}.

type_exprs({technicdef,_,TypeExprs,_,_}) -> TypeExprs.

meta_vars({technicdef,_,_,Metas,_}) -> Metas.

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
