-module(kraft).

-export([start/0]).
-export([t/0]).
-compile({parse_transform, do}).

-define(TEST_FILE, "test_check.k").
% -define(TEST_FILE, "test_parse.k").
start() ->
    application:start(kraft),
    leexyecc(),
    init:stop().


t() -> ok.

leexyecc() ->
    gerenate_lexer(),
    gerenate_parser(),
    File = priv(?TEST_FILE),
    BuildAllResult = do([error_m ||
        RawCode <- file:read_file(File),
        Tokens <- scan_kfile(binary_to_list(RawCode)),
        % {ok, log("Tokens:~n~w",[Tokens])},
        ParseTree <- kraft_parser:parse(Tokens),
        {ok, log("ParseTree:~n~p",[ParseTree])},
        Compiled <- kraft_compiler:compile(ParseTree),
        %% Loaded <- kraft:load_module(filename(File), Binary)
        ok
    ]),
    Ouput = case BuildAllResult
        of ok -> "Build Passed"
         ; {error,Reason} -> Reason
    end,
    log("Test Build : ~s",[Ouput]).

scan_kfile(BinString) ->
    case kraft_scanner:string(BinString)
        of {ok,Tokens,_EndLine} -> {ok,Tokens}
         ; Any -> Any
    end.

gerenate_lexer() ->
    log("Generating kraft lexer."),
    XRL = priv("kraft_scanner.xrl"),
    ScannerFile = filename:join([klib_dir(kraft), "src","klang","kraft_scanner.erl"]),
    error_logger:info_msg("Compiling lexer file ~p~n~n",[XRL]),
    {ok, ScannerFile} = leex:file(XRL, [{scannerfile, ScannerFile}]),
    recomp(kraft_scanner),
    log("Lexer Generated.").

gerenate_parser() ->
    log("Generating kraft parser."),
    YRL = priv("kraft_parser.yrl"),
    ParserFile = filename:join([klib_dir(kraft), "src","klang","kraft_parser.erl"]),
    error_logger:info_msg("Compiling parser file ~p~n~n",[YRL]),
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
        FL = fun({ErrorLine, Module, Reason}) ->
            [ F("line ~p : ",[ErrorLine])
            , yecc:format_error(Reason) ]
        end,
        Msg = [F("~p occured in file ~p~n",[Type, Filename])]
        ++ lists:map(FL, ErrorInfos),
        log(Msg)
    end.

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

priv(X) -> priv_file(kraft,X).
log(X) -> log(X,[]).
log(X,Y) -> error_logger:info_msg(string:join([X,"~n~n"],""),Y).
