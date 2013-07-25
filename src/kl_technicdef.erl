%% Gère les technicdefs alors qu'elles font encore partie du parse
%% tree, donc composées de tuples avec le type d'élément et les
%% numéros de lignes
-module(kl_technicdef).

-export([type_exprs/1,meta_vars/1,body/1,name/1,arity/1]).
-export([set_body/2]).
-export([composants/1,returntypes/1]).

-include_lib("kraftc/include/kraft_lang.hrl").

% technicdef() -> {technicdef,name,typeexprs,metas,body}.


type_exprs({technicdef,_,TypeExprs,_,_}) -> TypeExprs.
meta_vars({technicdef,_,_,Metas,_}) -> Metas.
body({technicdef,_,_,_,Body}) -> Body.
name({technicdef,{_,_,Name},_,_,_}) -> Name.

set_body({technicdef,X,Y,Z,_OldBody},Body) -> {technicdef,X,Y,Z,Body}.

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
    %% lists:usort -> remove duplicates
    TypesNames = lists:map(fun({return,TypeOutputs}) ->
                % [{aaaaa,TypeName }|| TypeName <- TypeOutputs]
                %% On sort ici pour remove les duplicates de types identiques mais pas dans le même ordre
                lists:sort([TypeName|| {{typeoutput,{typename,_,TypeName},_,_},_Props} <- TypeOutputs])
                % [{aaaaa,TypeName }|| {typeoutput,{typename,_,TypeName},_,_} <- TypeOutputs]
              end,
              Returns),
    lists:usort(TypesNames).


% {typeoutput,
%                               {typename,8,'Void'},
%                               'ANON',
%                               {number,8,0}}}]]},
