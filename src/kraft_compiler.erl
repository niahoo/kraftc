-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-include_lib("kraftc/include/kraft_lang.hrl").

compile(Filename) ->

    CompileResult = do([error_m ||
        RawCode <- file:read_file(Filename)
      , Tokens <- scan_kfile(binary_to_list(RawCode))
      % , {ok, log("Tokens:~n~w",[Tokens])}
      , ParseTree <- kl_parser:parse(Tokens)
      , KraftMod <-  kl_kraftmod:from_parsetree(ParseTree, Filename)
      % , {ok, log("KraftMod:~n~p",[KraftMod])},
      , klcheck_vardefs:check(KraftMod)
      , KraftModAllMatch <- klcheck_nomatchs:check(KraftMod)
      , klcheck_returns:check(KraftModAllMatch)
      , KraftModSigns <- kl_kraftmod:build_signatures(KraftModAllMatch)
      , KraftWithCore <- kl_codegen:build_forms(KraftModSigns)
      % , return(kl:log("Signatures ~p",[KraftWithCore#kraftmod.signatures]))
      % , return(kl:log("Forms ~p",[KraftWithCore#kraftmod.forms]))
      % , return(kl:log("Core Erlang ~s",[core_pp:format(KraftWithCore#kraftmod.forms)]))
      % , return(kl:string_to_paper(core_pp:format(KraftWithCore#kraftmod.forms)))
      % , return(kl:term_to_paper(KraftWithCore#kraftmod.forms))
      , _Linted <- lift(core_lint:module(KraftWithCore#kraftmod.forms))
      % , return(kl:log("Lint ~p",[Linted]))
      , build_beam(KraftWithCore)
      % , {ok,ok}
    ]).
    % CompiledModule.

build_beam(#kraftmod{filename=undefined}) ->
  {error, "Compiling a kraft module requires a filename"};
build_beam(#kraftmod{forms=Forms}=KraftMod) ->
    {ok,_ModuleName,Beam,Warnings} = compile:forms( Forms
           , [binary, from_core, return_errors, return_warnings]
                               ),
    [format_compiler_warnings(W) || W <- Warnings],
    {ok,Beam}.

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

scan_kfile(BinString) ->
    case kl_scanner:string(BinString)
        of {ok,Tokens,_EndLine} ->
            {ok,Tokens}
         ; {error,{Line,kl_scanner,Error},_EndLine} ->
            {error,kl_scanner:format_error(Error)
            ++ io_lib:format(" on line ~p",[Line])}
    end.
