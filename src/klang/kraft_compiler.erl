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
      , return(kl:log("Forms ~p",[KraftWithCore#kraftmod.forms]))
      , return(kl:log("Core Erlang ~s",[core_pp:format(KraftWithCore#kraftmod.forms)]))
      , Linted <- lift(core_lint:module(KraftWithCore#kraftmod.forms))
      , return(kl:log("Lint ~p",[Linted]))
      , KraftModBeam <- build_beam(KraftWithCore)
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

build_beam(#kraftmod{filename=undefined}) ->
  {error, "Compiling a kraft module requires a filename"};
build_beam(#kraftmod{forms=Forms, filename=Filename}=KraftMod) ->
   {ok,_ModuleName,Beam,Warnings} = compile:forms( Forms
                               , [binary, from_core, return_errors, return_warnings, {source, Filename}]
                               ),
   if
      length(Warnings) > 0 ->
        lists:map(kl:log("Warning : ~p",[_]), Warnings)
      ; true -> ok
  end,
  {ok,KraftMod#kraftmod{beam=Beam}}.

load_klmodule(#kraftmod{name=Name,beam=Beam,filename=Filename}=_KraftMod) ->
    {module, Name} = code:load_binary(Name,Filename,Beam),
    ok.


%% helper pour les monades -> force un 3 tuple en 2 tuple
lift({A, B}) -> {A,B};
lift({ErrorOK, A, B}) when ErrorOK =:= error ; ErrorOK =:= ok ->
  {ErrorOK, {A,B}}.
