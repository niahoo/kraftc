%%% Vérifie qu'il n'y ait pas de clauses dans un draw qui soient après
%%% une clause matchall. Dans le cas ou il n'y a pas de clause
%%% matchall, on ajoute également, les clauses renvoyant une erreur,
%%% par exemple draw_clause
-module(klcheck_nomatchs).

-export([check/1]).
% -compile({parse_transform, do}).

-include_lib("kraftc/include/kraft_lang.hrl").

check(#kraftmod{parsetree=ParseTree}=KM) ->
    {ok,Technicdefs} = kl_parsetree:technicdefs(ParseTree),
    Opti = [optimize(T,KM) || T <- Technicdefs],
    {ok,NewParseTree} = kl_parsetree:set_technicdefs(ParseTree,Opti),
    % kl:log("Nomatch parse tree ~p",[Opti]),
    {ok,KM#kraftmod{parsetree=NewParseTree}}.



optimize(TD,KM) ->
    Body = kl_technicdef:body(TD),
    NewBody = expr(Body,KM),
    NewTD = kl_technicdef:set_body(TD,NewBody),
    NewTD.

expr({draw,_ToMatch,Clauses},KM) ->
    {draw,_ToMatch,clauses(Clauses,KM)};

expr(X,_KM) -> X.

clauses([{{'_',_},_}=C|[]],_KM) ->
    %% matchall mais pas de clause suivante, c'est cool
    [C];
clauses([{{'_',Line},_}=C,Other|Clauses],KM) ->
    %% ici on a un matchall mais une clause ensuite. On écrit un
    %% warning et on ne retourne pas les clauses suivantes
    kl:log(
        "Warning in file ~p line ~p : Subsequent clauses will never match.",
        [filename:basename(KM#kraftmod.filename),Line]
    ),
    kl:log("Skipping ~p ",[Other|Clauses]),
    [C];
clauses([{{number,_,_}=ToBeat,CBody}|Clauses],KM) ->
    [{ToBeat,expr(CBody,KM)}|clauses(Clauses,KM)];
clauses([],_) -> [none_match_clause].

