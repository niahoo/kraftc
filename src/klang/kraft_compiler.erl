-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-include_lib("kraft/include/kraft_lang.hrl").

compile(KraftMod) ->

    CompileResult = do([error_m ||
        klcheck_vardefs:check(KraftMod)
      , KraftModSigns <- kl_kraftmod:build_signatures(KraftMod)
      , KraftWithCore <- kl_codegen:build_forms(KraftModSigns)
      , Linted <- core_lint:module(KraftWithCore#kraftmod.forms)
      , return(kl:log("Lint ~p",[Linted]))
      , return(kl:log("Core Erlang ~s",[core_pp:format(KraftWithCore#kraftmod.forms)]))
      , Beam <- get_beam(KraftWithCore)
      , KraftModBeam <- get_beam(KraftWithCore)
      , load_klmodule(KraftModBeam)
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

get_beam(#kraftmod{filename=undefined}) ->
  {error, "Compiling a kraft module requires a filename"};
get_beam(#kraftmod{forms=Forms, filename=Filename}=KraftMod) ->
   {ok,_ModuleName,Beam,Warnings} = compile:forms( Forms
                               , [binary, from_core, return_errors, return_warnings, {source, Filename}]
                               ),
   if
      length(Warnings) > 0 ->
        lists:map(kl:log("Warning : ~p",[_]), Warnings)
      ; true -> ok
  end,
  {ok,KraftMod#kraftmod{beam=Beam}}.

load_klmodule(#kraftmod{name=Name,beam=Beam,filename=Filename}=KraftMod) ->
    {module, Name} = code:load_binary(Name,Filename,Beam),
    ok.
