-module(kdict).
%%% Module permettant d'interagir avec les composants. Pour le moment
%%% on utilise simplement orddict
-export([is_key/2,
 size/1,
 fetch/2,
 fetch_keys/1,
 append/3,
 append_list/3,
 update/3,
 update/4,
 update_counter/3,
 fold/3,
 map/2,
 merge/3,
 erase/2,
 store/3,
 find/2,
 to_list/1,
 new/0,
 filter/2,
 from_list/1]).

-define(DICT,orddict).

is_key(A,B) -> ?DICT:is_key(A,B).
size(A) -> ?DICT:size(A).


% fetch(_,[]) -> undefined;
% fetch(Key,KlObj) -> ?DICT:fetch(Key,KlObj)
        % of Val} -> Val
         % ; _ -> undefined
    % end.
fetch(A,B) -> ?DICT:fetch(A,B).

fetch_keys(A) -> ?DICT:fetch_keys(A).
append(A,B,C) -> ?DICT:append(A,B,C).
append_list(A,B,C) -> ?DICT:append_list(A,B,C).
update(A,B,C) -> ?DICT:update(A,B,C).
update(A,B,C,D) -> ?DICT:update(A,B,C,D).
update_counter(A,B,C) -> ?DICT:update_counter(A,B,C).
fold(A,B,C) -> ?DICT:fold(A,B,C).
map(A,B) -> ?DICT:map(A,B).
merge(A,B,C) -> ?DICT:merge(A,B,C).
erase(A,B) -> ?DICT:erase(A,B).
store(A,B,C) -> ?DICT:store(A,B,C).
find(A,B) -> ?DICT:find(A,B).
to_list(A) -> ?DICT:to_list(A).
new() -> ?DICT:new().
filter(A,B) -> ?DICT:filter(A,B).
from_list(A) -> ?DICT:from_list(A).


