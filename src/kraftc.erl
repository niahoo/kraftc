-module(kraftc).
-compile({parse_transform,do}).
-export([main/1]).
-export([usage/0]).
-export([help/0]).
-record(todo,{kfiles=[],outdir}).
-define(ARGSPEC, [
        {kfile,  $f, "file",   string, "Files to compile : -f file1.k -f file2.k -f file3.k"}
      , {outdir, $o, "outdir", string, "Destination directory for binary files, default is \".\""}
    ]).

main(Args) ->

    case do([ error_m ||
              {Opts, Dummies} <- getopt:parse(?ARGSPEC,Args)
            , Todo <- 'what_to_do?'(Opts ++ Dummies)
            , do_what_to_do(Todo)
            ])
        of {ok, _Result} -> io:format("Compilation success~n",[])
         ; {error, Err} -> io:format("~n error : ~s~n",[format_error(Err)])
    end.


usage() ->
    getopt:usage(?ARGSPEC,"kraftc","",standard_io).

help() -> usage().

format_error({invalid_option,Opt}) -> io_lib:format("Invalid option ~p",[Opt]);
format_error({txt,String}) -> String;
format_error({Line,ChainTool,Error}) -> ChainTool:format_error(Error) ++ io_lib:format(" on line ~p",[Line]);
format_error(enoent) -> "Invalid file or directory";
format_error(X) -> io_lib:format("~s",[X]).

'what_to_do?'(Opts) ->
    % kl:logterm(Opts,"Opts"),
    'what_to_do?'(Opts,#todo{}).

'what_to_do?'([{kfile,Filename}|Opts], #todo{kfiles=KFiles}=Todo) ->
    'what_to_do?'(Opts, Todo#todo{kfiles=[Filename|KFiles]});

'what_to_do?'([{outdir,Outdir}|Opts], #todo{outdir=undefined}=Todo) ->
    'what_to_do?'(Opts, Todo#todo{outdir=Outdir});

'what_to_do?'([{outdir,Outdir}|Opts], #todo{outdir=AlreadyOutdir}=Todo) ->
    {error,{txt,io_lib:format("Invalid outdir ~s,~n already defined to ~s",[Outdir,AlreadyOutdir])}};

'what_to_do?'([],Todo) -> {ok,Todo};
'what_to_do?'([Other|_],Todo) -> {error,{invalid_option,Other}}.



do_what_to_do(#todo{outdir=undefined}=Todo) -> do_what_to_do(Todo#todo{outdir="."});
do_what_to_do(#todo{kfiles=[]}=Todo) -> {error,{txt,"Please specify one kraft file at least."}};
do_what_to_do(#todo{kfiles=Files,outdir=Outdir}=_Todo) ->
    %% Début de la compilation
    setenv(),
    CollectCompiled =
        fun (Filename,ModuleNames) ->
            case compile(Filename,Outdir)
                of {ok,ModName} -> {ok, [ModName|ModuleNames]}
                 ; Any -> Any
            end
        end,
    kl:monadic(CollectCompiled, [], Files).

compile(Filename,Outdir) ->
    Basename =  filename:basename(Filename,".k"),
    BinaryName = Basename ++ ".kb",
    do([error_m ||
        io:format(" -- compile ~s ",[Basename ++ ".k"]),
        Beam <- kraft_compiler:compile(Filename),
        file:write_file(filename:join([Outdir,BinaryName]),Beam),
        io:format("=> ~s~n",[BinaryName]),
        {ok, list_to_atom("km@" ++ Basename)}
    ]).

setenv() ->
    kl:start_uuid(),
    ok.
