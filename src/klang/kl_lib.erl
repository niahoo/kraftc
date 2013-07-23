-module(kl_lib).

%% Librairie de fonctions utilisateur utilisables dans les fichiers .k

% -export([t/0]).
-export([max/2,min/2]).
-export([getprop/2]).
-export([random/2]).
-export([iodump/1,iodump/2,iodump/3,iodump/4,iodump/5,iodump/6]).
% -export([sum/1,avg/1]).

max(A,B) -> erlang:max(A,B).
min(A,B) -> erlang:min(A,B).

getprop(Key,KlObj) ->
    Val = kdict:fetch(Key,KlObj),
    % kl:log("Yielding ~p = ~p from ~p",[Key,Val,KlObj]),
    Val.

random(To,From) when To > From -> random(From,To); %% Si les bornes sont en ordre inverse, on réinverse
random(From,To) ->
    %% random server uniform renvoie un nombre entre 1 et X inclus.
    %% Pour trouver un nombre entre X et Y il faut donc trouver l'écart (+1), soit (Z = Y - X + 1),
    %% Appeler uniform(Z) sur cet écart puis redécaler le résultat de From vers 1
    Range = To - From + 1,
    Rnd = random_server:uniform(Range),
    Rnd + From - 1.

%% La fonction iodump permet de débugger des valeurs et retourne la
%% dernière valeur scalaire kraft-compatible
iodump(A) -> iodump2([A]).
iodump(A,B) -> iodump2([A,B]).
iodump(A,B,C) -> iodump2([A,B,C]).
iodump(A,B,C,D) -> iodump2([A,B,C,D]).
iodump(A,B,C,D,E) -> iodump2([A,B,C,D,E]).
iodump(A,B,C,D,E,F) -> iodump2([A,B,C,D,E,F]).



iodump2(List) ->
    Print = lists:flatten(["Kraft var dump :", [" ~p" || _ <- List],"~n"]),
    spawn(fun() -> error_logger:info_msg(Print,List) end),
    lists:last(List).







% sum(List) when is_list(List) -> lists:sum(List).
% avg(List) when is_list(List) -> lists:sum(List) / length(List).

% t() -> avg([random(-5,-8) || _ <- lists:seq(0,1000)]).
