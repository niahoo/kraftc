-module(kraft_compiler).
-export([compile/2]).
-compile({parse_transform, do}).

compile(ParseTree,ModuleName) ->

    do([error_m ||
        klcheck_vardefs:check({parsetree,ParseTree}),
        klget:signatures(ParseTree)
    ]).

