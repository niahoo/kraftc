-module(kraft).

-export([start/0]).
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

leexyecc() ->
    gerenate_lexer(),
    gerenate_parser(),
    Filename = priv(?TEST_FILE),
    BuildAllResult = do([error_m ||
        RawCode <- file:read_file(Filename),
        Tokens <- scan_kfile(binary_to_list(RawCode)),
        % {ok, log("Tokens:~n~w",[Tokens])},
        ParseTree <- kraft_parser:parse(Tokens),
        KraftMod <-  kl_kraftmod:from_parsetree(ParseTree, Filename),
        % {ok, log("KraftMod:~n~p",[KraftMod])},
        _Compiled <-  kraft_compiler:compile(KraftMod),
        ok
    ]),
    Ouput = case BuildAllResult
        of ok -> "Build Passed"
         ; {error,Reason} -> Reason
    end,
    try
        log("Test Build : ~s",[Ouput])
    catch
        error:badarg -> log("Test Build : ~p",[Ouput])
    end,
    spawn( fun() -> kl:log("kl:t() ~p",[catch kl:t()]) end).


scan_kfile(BinString) ->
    case kraft_scanner:string(BinString)
        of {ok,Tokens,_EndLine} -> {ok,Tokens}
         ; Any -> Any
    end.

gerenate_lexer() ->
    log("Generating kraft lexer."),
    XRL = priv("kraft_scanner.xrl"),
    ScannerFile = filename:join([klib_dir(kraft), "src","klang","kraft_scanner.erl"]),
    kl:log("Compiling lexer file ~p",[XRL]),
    {ok, ScannerFile} = leex:file(XRL, [{scannerfile, ScannerFile}]),
    recomp(kraft_scanner),
    log("Lexer Generated.").

gerenate_parser() ->
    log("Generating kraft parser."),
    YRL = priv("kraft_parser.yrl"),
    ParserFile = filename:join([klib_dir(kraft), "src","klang","kraft_parser.erl"]),
    kl:log("Compiling parser file ~p",[YRL]),
    {ok, ParserFile, Warnings} = yecc:file(YRL, [{parserfile, ParserFile},{return_warnings,true},{report_warnings,false}]),
    lists:map(fun_print_yecc_error(warning), Warnings),
    recomp(kraft_parser),
    log("Parser Generated.").


recomp(Module) ->
    code:delete(Module),
    code:purge(Module),
    Filename = filename:join([klib_dir(kraft), "src","klang",atom_to_list(Module)]),
    Outdir = filename:join([klib_dir(kraft), "ebin"]),
    {ok, Module} = compile:file(Filename,[{outdir, Outdir}]),
    ok.


fun_print_yecc_error(Type) ->
    fun ({Filename, ErrorInfos}) ->
        F = fun io_lib:format/2,
        FL = fun({ErrorLine, _Module, Reason}) ->
            [ F("line ~p : ",[ErrorLine])
            , yecc:format_error(Reason) ]
        end,
        Msg = [F("~p occured in file ~p~n",[Type, Filename])]
        ++ lists:map(FL, ErrorInfos) ++ " \nend warnings",
        log(Msg)
    end.


klib_dir(X) -> kl:klib_dir(X).
priv(X) -> priv_file(kraft,X).
priv_file(App,X) -> kl:priv_file(App,X).
log(X) -> kl:log(X).
log(X,Y) -> kl:log(X,Y).

