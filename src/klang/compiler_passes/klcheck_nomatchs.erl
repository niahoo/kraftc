%%% Vérifie qu'il n'y ait pas de clauses dans un draw qui soient après
%%% une clause matchall
-module(klcheck_nomatchs).

-export([check/1]).
% -compile({parse_transform, do}).

-include_lib("kraft/include/kraft_lang.hrl").

check(#kraftmod{parsetree=ParseTree}=KM) ->
    {ok,Technicdefs} = kl_parsetree:technicdefs(ParseTree),
    Opti = [optimize(T,KM) || T <- Technicdefs],
    {ok,NewParseTree} = kl_parsetree:set_technicdefs(ParseTree,Technicdefs),
    {ok,KM#kraftmod{parsetree=NewParseTree}}.


body(TD) -> kl_technicdef:body(TD).
set_body(TD,Body) -> set_body(TD,Body).


optimize(TD,KM) ->
    Body = kl_technicdef:body(TD),
    NewBody = expr(Body,KM),
    NewTD = kl_technicdef:set_body(TD,NewBody),
    NewTD.



expr({draw,_ToMatch,Clauses},KM) ->
    {draw,_ToMatch,clauses(Clauses,KM)};

expr(X,KM) -> X.

clauses([{{otherwise,_},CBody}=C|[]],KM) ->
    %% matchall mais pas de clause suivante, c'est cool
    [C];
clauses([{{otherwise,Line},CBody}=C,Other|Clauses],KM) ->
    %% ici on a un matchall mais une clause ensuite. On écrit un
    %% warning et on ne retourne pas les clauses suivantes
    kl:log(
        "Warning in file ~p line ~p : Subsequent clauses will never match.",
        [filename:basename(KM#kraftmod.filename),Line]
    ),
    [C];
clauses([{{number,_,_}=ToBeat,CBody}=C|Clauses],KM) ->
    [{ToBeat,expr(CBody,KM)}|clauses(Clauses,KM)];
clauses([],_) -> [].

