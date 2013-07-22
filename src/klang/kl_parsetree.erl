-module(kl_parsetree).

-export([technicdefs/1]).
-export([set_technicdefs/2]).
-export([group_technicdefs/1]).

technicdefs(ParseTree) ->
    %% Pour le moment le parsetree n'est composé que d'une liste de
    %% technicdefs
    {ok, ParseTree}.

set_technicdefs(_ParseTree,Technicdefs) ->
    %% Pour le moment le parsetree n'est composé que d'une liste de
    %% technicdefs
    {ok,Technicdefs}.


%% Renvoie [{Name,Arity,[technicdefs]}]
group_technicdefs(ParseTree) ->
    {ok, TechnicDefs} = technicdefs(ParseTree),
    ReadTNameArity =
        fun(TD, Dict) ->
            TechnicName = kl_technicdef:name(TD),
            Arity = kl_technicdef:arity(TD),
            dict:append({TechnicName,Arity},TD,Dict)
        end,
    Grouped = dict:to_list(lists:foldl(ReadTNameArity,dict:new(),TechnicDefs)),
    %% @todo optimiser en virant les signatures identiques et en
    %% @émettant un ouarningue
    % kl:log("Grouped : ~p",[Grouped]),
    Grouped.
