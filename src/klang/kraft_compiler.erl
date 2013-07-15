-module(kraft_compiler).
-export([compile/1]).
-compile({parse_transform, do}).

compile(ParseTree) ->
    do([error_m ||
        klcheck_vardefs:check({parsetree,ParseTree})
    ]).

