-module(kl_kraftmod).

-export([from_parsetree/2]).
-export([build_signatures/1]).

-include_lib("kraft/include/kraft_lang.hrl").
from_parsetree(ParseTree, Name) ->
    {ok, #kraftmod{ parsetree=ParseTree , name=Name , step=1 }}.


build_signatures(KraftMod) ->
    kl:log("Building signatures for module ~p",[KraftMod#kraftmod.name]),
    {ok, TechnicDefs} = kl_parsetree:technicdefs(KraftMod#kraftmod.parsetree),
    {ok, KraftMod}.



