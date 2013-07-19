%% Gère les technicdefs alors qu'elles font encore partie du parse
%% tree, donc composées de tuples avec le type d'élément et les
%% numéros de lignes
-module(kl_technicdef).

-export([type_exprs/1,meta_vars/1,body/1,name/1,arity/1]).
-export([composants/1,returntypes/1]).

-include_lib("kraft/include/kraft_lang.hrl").

% technicdef() -> {technicdef,name,typeexprs,metas,body}.


type_exprs({technicdef,_,TypeExprs,_,_}) -> TypeExprs.
meta_vars({technicdef,_,_,Metas,_}) -> Metas.
body({technicdef,_,_,_,Body}) -> Body.
name({technicdef,{_,_,Name},_,_,_}) -> Name.

arity(TD) ->
    length(type_exprs(TD)).

composants({technicdef,_,TypeExprs,_,_}) ->
    lists:map(fun({typeinput,{typename,_,TypeName},_VarDef,Qtty}) ->
                case Qtty
                    of service -> {TypeName,service}
                     ; {number,_,Number} -> {TypeName,Number}
                end
              end,
              TypeExprs).

returntypes({technicdef,_,_,_,Body}) ->
    Returns = kl:find2tuples(return,Body),
    lists:map(fun({return,TypeOutputs}) ->
                % [{aaaaa,TypeName }|| TypeName <- TypeOutputs]

                [TypeName|| {{typeoutput,{typename,_,TypeName},_,_},_Props} <- TypeOutputs]
                % [{aaaaa,TypeName }|| {typeoutput,{typename,_,TypeName},_,_} <- TypeOutputs]
              end,
              Returns).


% {typeoutput,
%                               {typename,8,'Void'},
%                               'ANON',
%                               {number,8,0}}}]]},
