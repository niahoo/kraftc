-module(kl_parsetree).

-export([technicdefs/1]).

technicdefs(ParseTree) ->
    %% Pour le moment le parsetree n'est composé que d'une liste de technicdefs
    {ok, ParseTree}.

