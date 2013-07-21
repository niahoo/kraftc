-module(klcheck_vardefs).
-export([check/1]).
-compile({parse_transform, do}).

-include_lib("kraft/include/kraft_lang.hrl").

check(#kraftmod{parsetree=ParseTree}) ->
    check_undefined_vars(ParseTree).

%% Check undef vars -------------------------------------------------

check_undefined_vars([]) -> ok;
check_undefined_vars([TechnicDef|ParseTree]) ->
    % kl:log("Checking vardefs in kt@~p",[kl_technicdef:name(TechnicDef)]),
    case check_undefined_vars2(TechnicDef)
        of ok -> check_undefined_vars(ParseTree)
         ; Any -> Any
    end.

%% On va vérifier que toutes les variables utilisées sont bien
%% déclarées au préalable
check_undefined_vars2(TechnicDef) ->
    do([error_m ||
        %% Récup des noms définis dans les types
        TypeVars <- get_type_vars(kl_technicdef:type_exprs(TechnicDef), []),
        %% Récup des noms définis dans les meta (avec comme acc de base ceux des types)
        AllDef <- get_meta_vars(kl_technicdef:meta_vars(TechnicDef), TypeVars),
        check_body(kl_technicdef:body(TechnicDef),AllDef)
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
    VarsUsed = get_all_vars(Expr),
    % case VarsUsed
        % of [] -> kl:log("No vars used in metadefs")
         % ; _ -> kl:log("Checking vars used in metadefs")
    % end,
    case ensure_all_member(VarsUsed,Acc)
        of {error,Reason} -> {error,Reason}
         ; ok -> get_meta_vars(Ms,[Name|Acc])
    end.

check_body(Body,Vars) ->
    VarsUsed = get_all_vars(Body),
    % case VarsUsed
        % of [] -> kl:log("No vars used in body")
         % ; _ -> kl:log("Checking vars used in body")
    % end,
    ensure_all_member(VarsUsed,Vars).

%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------

%% renvoie tous les names trouvés. On cherche simplement un 3-tuple
%% commençant par name. Pour simplifier, on peut aussi renvoyer nil.
%% à la fin la liste est flattened et traversée pour virer les nil
get_all_vars(X) -> kl:find3tuples(var,X).



%% Vérifie que pour tout element {X,_} de Elems, X est dans la liste Pool
ensure_all_member(Elems,Pool) ->
    %% On utilise une simple monade : si on renvoie {error,Reason} le
    %% reste des éléments n'est pas évalué, l'erreur traverse le reste
    %% du Pool
    MicroMonad =
                fun (_Elem, {error,Reason}=Lift) ->
                        Lift
                  ; ({var,Line,Name},  ok) when is_atom(Name), is_integer(Line)->
                        % kl:log("Check if ~p is defined ... ",[Name],nnl),
                        case lists:member(Name,Pool)
                            of true ->
                                % kl:log("yes"),
                                ok
                             ; false ->
                                % kl:log("no"),
                                {error, io_lib:format("Undefined var ~p on line ~p",[Name,Line])}
                        end
                end,
    lists:foldl(MicroMonad,ok,Elems).
