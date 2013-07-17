-module(kl_kraftmod).

-export([from_parsetree/2]).
-export([build_signatures/1]).

-include_lib("kraft/include/kraft_lang.hrl").
from_parsetree(ParseTree, Filename) ->
    Name = module_name(Filename),
    {ok, #kraftmod{ parsetree=ParseTree ,filename=Filename, name=Name , step=1 }}.


build_signatures(#kraftmod{parsetree=ParseTree}=KraftMod) ->
    kl:log("Building signatures for module ~p",[KraftMod#kraftmod.name]),
    {ok, TechnicDefs} = kl_parsetree:technicdefs(ParseTree),
    Signatures = lists:foldl(fun(TD,Dict) ->
                                TechnicName = kl_technicdef:name(TD),
                                Composants = kl_technicdef:composants(TD),
                                ReturnTypes = kl_technicdef:returntypes(TD),
                                orddict:append(TechnicName,{Composants,ReturnTypes},Dict)
                             end,
                             orddict:new(),
                             TechnicDefs),
    %% @todo optimiser en virant les signatures identiques et en
    %% @émettant un ouarningue
    kl:log("Signatures : ~p",[Signatures]),

    {ok, KraftMod#kraftmod{signatures=Signatures,step=2}}.


module_name(Filename) -> list_to_atom("km@" ++ filename:basename(Filename,".k")).
