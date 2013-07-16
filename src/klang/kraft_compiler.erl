-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).

-include_lib("kraft/include/kraft_lang.hrl").

compile(KraftMod) ->

    CompileResult = do([error_m ||
        klcheck_vardefs:check(KraftMod)
      , KraftMod2 <- kl_kraftmod:build_signatures(KraftMod)
      , KraftMod2
    ]),
    kl:log("Compile result ~n~p",[CompileResult]),
    case CompileResult
        of {error,Reason} -> {error,Reason}
         ; #kraftmod{}=KM -> {ok,KM}
         % ; Any -> {error,Any}
    end.
    % CompiledModule.

