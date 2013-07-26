%% Vérifie que toutes les technicdefs retournent bien des typeresults
%% via la fonction must qui parcourt l'arbre de haut en bas. Parfois,
%% on utilisera la fonction must_not qui, elle, s'assure que le sous
%% arbre ne renvoie PAS un type mais une simple expression. C'est le
%% cas pour le palier d'une clause draw.

-module(klcheck_returns).

-export([check/1]).
-compile({parse_transform, do}).
-define(ERROR_LABEL, "A technic body must only result in an item type,").
-include_lib("kraftc/include/kraft_lang.hrl").

check(#kraftmod{parsetree=ParseTree}) ->
    {ok,Technicdefs} = kl_parsetree:technicdefs(ParseTree),
    kl:monadic(fun (Elem,_) -> technicdef(Elem) end, any, Technicdefs).
    % {ok,NewParseTree} = kl_parsetree:set_technicdefs(ParseTree,Opti),
    % kl:log("Nomatch parse tree ~p",[Opti]),
    % {ok,KM#kraftmod{parsetree=NewParseTree}}.


technicdef(TD) ->
    Body = kl_technicdef:body(TD),
    must(Body).

must({draw,_Matcher, Clauses}) ->
    kl:monadic(fun (Elem,_) -> drawclause(Elem) end, any, Clauses);
must({number,L,X}) ->
    {error, io_lib:format(?ERROR_LABEL "number ~p found, line ~p",[X,L])};
must({string,L,S}) ->
    {error, io_lib:format(?ERROR_LABEL "'~s' found, line ~p",[S,L])};
must({var,L,Name}) ->
    {error, io_lib:format(?ERROR_LABEL "variable '~s' found, line ~p",[Name,L])};
must({call,getprop,[{key,_l,Key},{var,L,Name}]}) ->
    {error, io_lib:format(?ERROR_LABEL "property '~s.~s' found, line ~p",[Name,Key,L])};
must({call,{name,L,Name},_Args}) ->
    {error, io_lib:format(?ERROR_LABEL "call to '~s' found, line ~p",[Name,L])};
must({return,_}) ->
    ok;
must(X) ->
    kl:term_to_paper(X),
    ok.

must_not({number,L,X}) ->
    ok;
must_not(X) ->
    kl:term_to_paper(X),
    ok.

drawclause({ToBeat,Result}) ->
    do([error_m ||
        must_not(ToBeat),
        must(Result)
    ]).
