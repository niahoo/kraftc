-module(random_server).

-export([start_link/0]).
-export([uniform/0,string/2,list/2,uniform/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

uniform() ->
    gen_server:call(?MODULE, uniform).

uniform(Max) ->
    gen_server:call(?MODULE, {uniform, Max}).

string(Length, Chars) ->
    %% Le même caractère peut sortir plusieurs fois
    gen_server:call(?MODULE, {string, Length, Chars}).

%% récupère Amount items dans la liste aléatoirement
list(Amount, List) ->
    %% Un item ne peut être tiré qu'une fois, on va donc shuffle la
    %% list puis en récupérer les Amount premiers éléments
    L = length(List),
    %% On crée une liste de nombres dans un ordre aléatoire à l'aide
    %% la la fonction string. Attention, elle peut renvoyer plusieurs
    %% fois le même nombre dans la liste, et du coup le shuffle n'est
    %% pas vraiment naturel car entre deux nombre de ^même index ils
    %% restent dans le même ordre l'un envers l'autre
    ShuffleIndexes = ?MODULE:string(L, lists:seq(1,L)),
    %% On attache chaque index à un élément de notre liste
    ShuffleZip = lists:zip(ShuffleIndexes, List),
    SortedZip = lists:sort(ShuffleZip),
    %% Avec sublist() on récupère une site de la taille voulue, les N premiers
    [Elem || {_I, Elem} <- lists:sublist(SortedZip, Amount)].

%% Server implementation, a.k.a.: callbacks

init([]) ->
    random:seed(erlang:now()),
    error_logger:info_msg("Random server started~n~n"),
    State = state([]),
    {ok, State}.


handle_call(uniform, _From, State) ->
    {reply, random:uniform(), State};

handle_call({uniform, Max}, _From, State) ->
    {reply, random:uniform(Max), State};


handle_call({string, Length, Chars}, _From, State) ->
    Len = length(Chars),
    Str = lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(Len),Chars)] ++ Acc
                      end, [], lists:seq(1, Length)),
    {reply, Str, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -----------------------------------------------------------------

%% Sert juste à checker la consistence du nombre d'entrées dans le
%% tuple
state([]=State) -> State.

% state(_State) -> error_logger:info_msg("Inconsistent random_server State : ~p~n~n",[_State%]).
