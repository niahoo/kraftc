-module(kl).
-export([log/1,log/2,log/3,to_list/1]).
-export([find2tuples/2,find3tuples/2]).

% -define(LOG,error_logger:info_msg).
-define(LOG,io:format).
% -define(LOGLN," ").
-define(LOGLN,"~n").
% -define(LOGLN,"~n~n").

-compile({parse_transform, cut}).

%% nnl signifie "no newline"
log(X) -> log(X,[]).
log(X,nnl) -> log(X,[],nnl);
log(X,Y) ->
    ?LOG(string:join([X,?LOGLN],""),Y).
log(X,Y,nnl) ->
    ?LOG(X,Y).


find3tuples(Tag,X) ->
    % log("looking for {~p,_,_} ",[Tag]),
    All = f3t(Tag,X),
    % log("3tups : ~p",[All]),
    lists:filter( fun ({Tag1,_,_}=Tuple) when Tag1 =:= Tag -> true
                    ; (_) -> false
                  end,
                  lists:flatten(All)).

f3t(Tag, {Tag,_,_}=Tuple) -> [Tuple];

f3t(Tag, List) when is_list(List) ->
    lists:map(f3t(Tag,_), List);

f3t(Tag,Tuple) when is_tuple(Tuple) ->
    % log("inspect ~p",[Tuple]),
    f3t(Tag,tuple_to_list(Tuple));

f3t(_,_) -> [].

find2tuples(Tag,X) ->
    % log("looking for {~p,_,_} ",[Tag]),
    All = f2t(Tag,X),
    % log("2tups : ~p",[All]),
    lists:filter( fun ({Tag1,_}=Tuple) when Tag1 =:= Tag -> true
                    ; (_) -> false
                  end,
                  lists:flatten(All)).

f2t(Tag, {Tag,_}=Tuple) -> [Tuple];

f2t(Tag, List) when is_list(List) ->
    lists:map(f2t(Tag,_), List);

f2t(Tag,Tuple) when is_tuple(Tuple) ->
    % log("inspect ~p",[Tuple]),
    f2t(Tag,tuple_to_list(Tuple));

f2t(_,_) -> [].


to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_list(X)    -> X.
