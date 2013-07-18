-module(kl_codegen).

-include_lib("kraft/include/kraft_lang.hrl").
-compile({parse_transform, cut}).

-export([build_forms/1]).

build_forms(#kraftmod{signatures=undefined}) ->
    { error,
      io_lib:format("Signatures are required to build forms",[]) };
build_forms(#kraftmod{signatures=Signatures}=KraftMod) ->
    LitModuleName = cerl:c_atom(KraftMod#kraftmod.name),
    ModuleInfoFuns = get_module_info(LitModuleName),
    TechnicInfos = get_technic_infos(Signatures),
    Module = cerl:c_module( LitModuleName
                          , [ cerl:c_fname(module_info,0)
                            , cerl:c_fname(module_info,1)
                            , cerl:c_fname(technics_infos,0)
                            , cerl:c_fname(technic_infos,1)
                            ]
                          , lists:append([
                              ModuleInfoFuns
                           ,  TechnicInfos
                            ])

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

get_technic_infos(Signatures) ->
    Key = cerl:c_var('Key'),
    [ { cerl:c_fname(technics_infos,0)
      , cerl:c_fun([], signatures_literal(Signatures))
      }
    , { cerl:c_fname(technic_infos,1)
      , cerl:c_fun([Key], signatures_literal_case(Signatures,Key))
      } ].

ccall(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    LitM = cerl:c_atom(M),
    LitF = cerl:c_atom(F),
    cerl:c_call(LitM, LitF, A).

signatures_literal(Signatures) ->
    deep_literal(Signatures).

signatures_literal_case(Signatures,Arg) ->
    MkCaseClause =
        fun({TechnicName,TechnicClauses}) ->
            cerl:c_clause([cerl:c_atom(TechnicName)], deep_literal(TechnicClauses))
        end,
    WildCardVar = cerl:c_var('_Any'),
    WildCardClause = cerl:c_clause([WildCardVar], cerl:c_atom(undefined)),
    CaseClauses = [MkCaseClause(S) || S <- Signatures] ++ [WildCardClause],
    cerl:c_case(Arg,CaseClauses).


deep_literal(List) when is_list(List) ->
    kl:log("lit. ~w",[List]),
    cerl:make_list(lists:map(fun deep_literal/1, List));

deep_literal(Tuple) when is_tuple(Tuple) ->
    kl:log("lit. ~w",[Tuple]),
    List = tuple_to_list(Tuple),
    Literals = lists:map(fun deep_literal/1, List),
    cerl:c_tuple(Literals);

deep_literal(Atom) when is_atom(Atom) ->
    kl:log("lit. ~w",[Atom]),
    cerl:c_atom(Atom);

deep_literal(Float) when is_float(Float) ->
    kl:log("lit. ~w",[Float]),
    cerl:c_float(Float);

deep_literal(Int) when is_integer(Int) ->
    kl:log("lit. ~w",[Int]),
    cerl:c_int(Int).
