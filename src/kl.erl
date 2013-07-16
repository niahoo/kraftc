-module(kl).
-export([log/1,log/2,find3tuples/2]).

% -define(LOG,error_logger:info_msg).
-define(LOG,io:format).

-compile({parse_transform, cut}).

log(X) -> log(X,[]).
log(X,Y) ->
    ?LOG(string:join([X,"~n~n"],""),Y).


find3tuples(Tag,X) ->
    % log("looking for {~p,_,_} ",[Tag]),
    All = gan(Tag,X),
    % log("3tups : ~p",[All]),
    lists:filter( fun ({Tag1,_,_}=Tuple) when Tag1 =:= Tag -> true
                    ; (_) -> false
                  end,
                  lists:flatten(All)).

gan(Tag, {Tag,_,_}=Tuple) -> Tuple;

gan(Tag, List) when is_list(List) ->
    lists:map(gan(Tag,_), List);

gan(Tag,Tuple) when is_tuple(Tuple) ->
    % log("inspect ~p",[Tuple]),
    gan(Tag,tuple_to_list(Tuple));

gan(_,_) -> nil.
