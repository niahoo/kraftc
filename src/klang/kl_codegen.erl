-module(kl_codegen).

-include_lib("kraft/include/kraft_lang.hrl").
-compile({parse_transform, cut}).

-define(INPUT_STEP,2).

-export([build_forms/1]).

build_forms(#kraftmod{signatures=undefined}) ->
    { error,
      io_lib:format("Signatures are required to build forms",[]) };
build_forms(#kraftmod{step=?INPUT_STEP}=KraftMod) ->
    LitName = cerl:c_atom(KraftMod#kraftmod.name),
    ModuleInfoFuns = get_module_info(LitName),
    Module = cerl:c_module( LitName
                          , [cerl:c_fname(module_info,0),cerl:c_fname(module_info,1)]
                          , ModuleInfoFuns
                          ),
    {ok,KraftMod#kraftmod{forms=Module}}.



get_module_info(LitName) ->
    Key = cerl:c_var('Key'),
    [ { cerl:c_fname(module_info,0)
      , cerl:c_fun([], ccall(erlang,get_module_info,[LitName]))
      }
    , { cerl:c_fname(module_info,1)
      , cerl:c_fun([Key], ccall(erlang,get_module_info,[LitName,Key]))
      }
    ].

ccall(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    LitM = cerl:c_atom(M),
    LitF = cerl:c_atom(F),
    cerl:c_call(LitM, LitF, A).
