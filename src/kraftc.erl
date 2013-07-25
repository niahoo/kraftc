-module(kraftc).
-export([main/1]).

-define(ARGSPEC, [
        {kfile,  $f, "file",   string, "Files to compile : -f file1.k -f file2.k -f file3.k"}
      , {outdir, $o, "outdir", string, "Destination directory for binary files, default is \".\""}
    ]).

main(Args) ->
    usage(), ok.


usage() ->
    getopt:usage(?ARGSPEC,"kraftc","",standard_io).
