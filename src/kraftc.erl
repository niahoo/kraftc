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
        of {ok, Result} -> io:format("Compilation success, ~p~n",[Result])
         ; {error, Err} -> io:format("~s~n",[format_error(Err)])
    end.


usage() ->
    getopt:usage(?ARGSPEC,"kraftc","",standard_io).

help() -> usage().

format_error({invalid_option,Opt}) -> io_lib:format("Invalid option ~p",[Opt]);
format_error({txt,String}) -> String;
format_error(X) -> io_lib:format("~p",X).

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



do_what_to_do(Todo) ->
    kl:logterm(Todo,"Todo !"),
    {ok,lol}.
