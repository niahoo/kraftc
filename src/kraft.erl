-module(kraft).

-export([start/0]).
-export([initstop/0]).
-export([t/0]).
-compile({parse_transform, do}).

-define(TEST_FILE, "test_check.k").
% -define(TEST_FILE, "test_parse.k").
start() ->
    application:start(kraft),
    kl:start_uuid(),
    leexyecc().

-include_lib("kraft/include/kraft_lang.hrl").

t() -> ok.
initstop() ->
    receive after 200 -> init:stop() end.

leexyecc() ->
    Filename = priv(?TEST_FILE),
    BuildAllResult = kraft_compiler:compile(Filename),
    case BuildAllResult
        of {ok, KraftMod} ->
            log("Test Build : Build Passed"),
            % spawn( fun() -> kl:log("kl:t() ~p",[catch kl:t()]) end),
            ok
         ; {error,Reason} ->
            case catch log("Test Build : Error, ~s",[Reason])
                of ok -> ok
                 ; _ -> log("Test Build : Error ~p",[Reason])
            end
         ; Other -> log("Test Build : Undefined ~p",[Other])
    end.





recomp(Module) ->
    code:delete(Module),
    code:purge(Module),
    Filename = filename:join([klib_dir(kraft), "src",atom_to_list(Module)]),
    Outdir = filename:join([klib_dir(kraft), "ebin"]),
    {ok, Module} = compile:file(Filename,[{outdir, Outdir}]),
    ok.




klib_dir(X) -> kl:klib_dir(X).
priv(X) -> priv_file(kraft,X).
priv_file(App,X) -> kl:priv_file(App,X).
log(X) -> kl:log(X).
log(X,Y) -> kl:log(X,Y).

