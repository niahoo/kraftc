-module(kl_parsetree).

-export([technicdefs/1]).
-export([group_technicdefs/1]).

technicdefs(ParseTree) ->
    %% Pour le moment le parsetree n'est composé que d'une liste de technicdefs
    {ok, ParseTree}.

%% Renvoie [{Name,Arity,[technicdefs]}]
technicdefs(ParseTree) ->
    TechnicDefs = technicdefs(ParseTree), amoispa().
