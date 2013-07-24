%% Vérifie que toutes les expressions retournées sont bien des
%% Typesresult

-module(klcheck_returns).

-export([check/1]).
% -compile({parse_transform, do}).

-include_lib("kraft/include/kraft_lang.hrl").

check(#kraftmod{parsetree=ParseTree}=KM) ->
    {ok,Technicdefs} = kl_parsetree:technicdefs(ParseTree),
    kl:monadic(fun (Elem,_) -> expr(Elem) end, any, Technicdefs).
    % {ok,NewParseTree} = kl_parsetree:set_technicdefs(ParseTree,Opti),
    % kl:log("Nomatch parse tree ~p",[Opti]),
    % {ok,KM#kraftmod{parsetree=NewParseTree}}.


expr(X) ->
    kl:term_to_paper(X),
    ok.

