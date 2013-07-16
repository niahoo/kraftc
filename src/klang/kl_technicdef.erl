-module(kl_technicdef).

-export([type_exprs/1,meta_vars/1,body/1,name/1]).

-include_lib("kraft/include/kraft_lang.hrl").



type_exprs({technicdef,_,TypeExprs,_,_}) -> TypeExprs.
meta_vars({technicdef,_,_,Metas,_}) -> Metas.
body({_,_,_,_,Body}) -> Body.
name({_,Name,_,_,_}) -> Name.

% technicdef() -> {technicdef,name,typeexprs,metas,body}.
