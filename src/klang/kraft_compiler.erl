-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).

-include_lib("kraft/include/kraft_lang.hrl").

compile(KraftMod) ->

    CompileResult = do([error_m ||
        klcheck_vardefs:check(KraftMod)
      , KraftModSigns <- kl_kraftmod:build_signatures(KraftMod)
      , KraftWithCore <- kl_codegen:build_forms(KraftModSigns)
      , Linted <- core_lint:module(KraftWithCore#kraftmod.forms)
      , return(kl:log("Lint ~p",[Linted]))
      , return(kl:log("Core Erlang ~s",[core_pp:format(KraftWithCore#kraftmod.forms)]))
      , get_beam(KraftWithCore)
    ]),
    try
        kl:log("Compile result ~n~s",[CompileResult])
    catch
        error:badarg -> kl:log("Compile result ~n~p",[CompileResult])
    end,
    case CompileResult
        of {error,Reason} -> {error,Reason}
         ; #kraftmod{}=KM -> {ok,KM}
         ; Any -> {error,Any}
    end.
    % CompiledModule.

get_beam(#kraftmod{forms=Forms}) -> ok.
