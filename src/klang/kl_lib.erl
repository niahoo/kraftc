-module(kl_lib).

%% Librairie de fonctions utilisateur utilisables dans les fichiers .k

-export([max/2,min/2]).
-export([getprop/2]).

max(A,B) -> erlang:max(A,B).
min(A,B) -> erlang:min(A,B).

getprop(Key,KlObj) ->
    Val = kdict:fetch(Key,KlObj),
    kl:log("Yielding ~p = ~p from ~p",[Key,Val,KlObj]),
    Val.

