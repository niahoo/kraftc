-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-include_lib("kraft/include/kraft_lang.hrl").

compile(KraftMod) ->

    CompileResult = do([error_m ||
        klcheck_vardefs:check(KraftMod)
      , KraftModAllMatch <- klcheck_nomatchs:check(KraftMod)
      , KraftModSigns <- kl_kraftmod:build_signatures(KraftModAllMatch)
      , KraftWithCore <- kl_codegen:build_forms(KraftModSigns)
      , return(kl:log("Signatures ~p",[KraftWithCore#kraftmod.signatures]))
      % , return(kl:log("Forms ~p",[KraftWithCore#kraftmod.forms]))
      % , return(kl:log("Core Erlang ~s",[core_pp:format(KraftWithCore#kraftmod.forms)]))
      , return(kl:string_to_paper(core_pp:format(KraftWithCore#kraftmod.forms)))
      % , return(kl:term_to_paper(KraftWithCore#kraftmod.forms))
      , Linted <- lift(core_lint:module(KraftWithCore#kraftmod.forms))
      , return(kl:log("Lint ~p",[Linted]))
      , KraftModBeam <- build_beam(KraftWithCore)
      , load_klmodule(KraftModBeam)
      , (KraftModBeam)
    ]),
    case CompileResult
        of {error,Reason} -> {error,Reason}
         ; #kraftmod{}=KM -> {ok,KM}
         ; Any -> {error,Any}
    end.
    % CompiledModule.

build_beam(#kraftmod{filename=undefined}) ->
  {error, "Compiling a kraft module requires a filename"};
build_beam(#kraftmod{forms=Forms}=KraftMod) ->
    {ok,_ModuleName,Beam,Warnings} = compile:forms( Forms
                               , [binary, from_core, return_errors, return_warnings]
                               ),
    [format_compiler_warnings(W) || W <- Warnings],
    {ok,KraftMod#kraftmod{beam=Beam}}.

load_klmodule(#kraftmod{name=Name,beam=Beam,filename=Filename}=_KraftMod) ->
    {module, Name} = code:load_binary(Name,Filename,Beam),
    {ok,Name}.


%% helper pour les monades -> force un 3 tuple en 2 tuple
lift({A, B}) -> {A,B};
lift({ErrorOK, A, B}) when ErrorOK =:= error ; ErrorOK =:= ok ->
    {ErrorOK, {A,B}}.

format_compiler_warnings({Filename,Ws}) ->
    Format =
        fun(Err) ->
            {Line, Mod, Desc} = case Err
                of {ErrorLine, Module, ErrorDescriptor} -> {ErrorLine, Module, ErrorDescriptor}
                 ; {Module, ErrorDescriptor} -> {none, Module, ErrorDescriptor}
            end,
            Description = Mod:format_error(Desc),
            kl:log("Warning on line ~p in file ~p, " ++ Description,[Line,Filename])
        end,
    [try
        Format(W)
    catch
        _:_ -> kl:log("Warning : ~p",[W])
    end || W <- Ws].
