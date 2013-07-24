-module(kl).
-export([t/0]).
-export([log/1,log/2,log/3,logterm/2,to_list/1]).
-export([find2tuples/2,find3tuples/2]).
-export([start_uuid/0,uuid/0]).
-export([unok/1]).
-export([monadic/3]).
-export([string_to_paper/1]).
-export([term_to_paper/1]).
-export([priv_file/1,priv_file/2,klib_dir/1]).


% -define(LOG,error_logger:info_msg).
-define(LOG,io:format).
% -define(LOGLN," ").
-define(LOGLN,"~n").
% -define(LOGLN,"~n~n").

-compile({parse_transform, cut}).


% t() -> km@test_check:crush({'Ble',varBle}).
t() ->
    Agriculteur = kdict:from_list([
        {skill_agriculture,5}
    ]),
    Ble = kdict:from_list([
        {conservation,3}
    ]),
    %  km@test_check:crush({'Agriculteur',Agriculteur},{'Ble',Ble}).
    % VieuxPain = kdict:from_list([{quality,3}]),
    % km@test_check:pain({'VieuxPain',VieuxPain}).
    % Agriculteurs = kdict:from_list([
        % {faim,100}
    % ]),
     % km@test_check:revolution({'Agriculteurs',Agriculteurs}).
     kl:log("kl:t ~p",[km@test_check:crush({'Agriculteur',Agriculteur},{'Ble',Ble})]).

%% nnl signifie "no newline"
log(X) -> log(X,[]).
log(X,nnl) -> log(X,[],nnl);
log(X,Y) ->
    ?LOG(string:join([X,?LOGLN],""),Y).
log(X,Y,nnl) ->
    ?LOG(X,Y).

logterm(Term,Label) ->
    log(Label ++ ": ~p",[Term]).

find3tuples(Tag,X) ->
    % log("looking for {~p,_,_} ",[Tag]),
    All = f3t(Tag,X),
    % log("3tups : ~p",[All]),
    lists:filter( fun ({Tag1,_,_}) when Tag1 =:= Tag -> true
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
    lists:filter( fun ({Tag1,_}) when Tag1 =:= Tag -> true
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


%% @todo gen server ?
start_uuid() ->
    register(kl_uuid_server, spawn( fun() -> uuid_loop(1) end )).

uuid_loop(X) ->
    receive {get_uuid, From} -> From ! {uuid,X} end,
    uuid_loop(X+1).

uuid() ->
    kl_uuid_server ! {get_uuid,self()},
    receive
        {uuid,UUID} -> UUID
    after 1000
        -> error(uuid_timeout)
    end.


unok({ok,V}) -> V.

%% sert à créer des fold monadiques sur des listes : La fonction de
%% rappel doit un acc wrappé dans {ok, Acc} ou bien {error, Reason}.
%% Si {error, Reason} est renvoyé alors les autres éléments ne sont
%% plus parcourus et {error, Reason} est le résultat final.
monadic(Fun,Acc0,List) -> monadic_2(Fun,{ok,Acc0},List).
monadic_2(_Fun, Acc, []) -> Acc;
monadic_2(Fun, {ok,Acc}, [H|T]) -> monadic_2(Fun,Fun(H,Acc),T);
monadic_2(_Fun, {error,_Reason}=Lift, _) -> Lift.

%% écrit un terme erlang vers le fichier priv/paper
string_to_paper(IOList) ->
    Path = priv_file("paper"),
    file:write_file(Path,iolist_to_binary(IOList)).
term_to_paper(Term) ->
    Path = priv_file("paper"),
    file:write_file(Path,io_lib:format("~p",[Term])).

priv_file(File) -> priv_file(kraft,File).
priv_file(App, File) when is_atom(App), is_list(File) ->
    PrivDir = case code:priv_dir(App)
        of {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:join(filename:dirname(Ebin), "priv")
         ; Dir -> Dir
    end,
    filename:join(PrivDir, File).

klib_dir(App) ->
    case code:lib_dir(App)
        of {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:dirname(Ebin)
        ; LibDir -> LibDir
    end.
