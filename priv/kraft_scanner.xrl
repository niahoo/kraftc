Definitions.

EXP = [eE][0-9]+
FLO = \.[0-9]+
INT = [0-9]+
NT  = [a-z_][A-Za-z_]*
NB  = [A-z][A-Za-z_]*
WS  = ([\000-\s]|%.*|--[^\n]*\n)

Rules.

[>][>]     : {token,{list_to_atom(TokenChars),TokenLine}}.
[-][>]     : {token,{list_to_atom(TokenChars),TokenLine}}.
end        : {token,{list_to_atom(TokenChars),TokenLine}}.
draw       : {token,{list_to_atom(TokenChars),TokenLine}}.
otherwise  : {token,{list_to_atom(TokenChars),TokenLine}}.
{INT}{FLO} : {token,{number,TokenLine,to_num(TokenChars)}}.
{INT}{EXP} : {token,{number,TokenLine,to_num(tofloatexp(TokenChars))}}.
{INT}{FLO}{EXP} : {token,{number,TokenLine,to_num(TokenChars)}}.
{INT}      : {token,{number,TokenLine,to_num(TokenChars)}}.
{NT}+      : {token,{name,TokenLine,list_to_atom(TokenChars)}}.
{NB}+      : {token,{typename,TokenLine,list_to_atom(TokenChars)}}.
\+         : {token,{'+',TokenLine}}.
-          : {token,{'-',TokenLine}}.
\*         : {token,{'*',TokenLine}}.
/          : {token,{'/',TokenLine}}.
[<>(),:={}\.]      : {token,{list_to_atom(TokenChars),TokenLine}}.
{WS}+      : skip_token.

Erlang code.

atom(TokenChars) -> list_to_atom(TokenChars).

to_num(X) when is_list(X) ->
    try
        list_to_float(X)
    catch
        error:badarg -> list_to_integer(X)
    end.


%% Convertit une exponentielle entière en float
tofloatexp(X) -> binary_to_list(iolist_to_binary(tofloatexp2(X))).
tofloatexp2([]) -> [];
tofloatexp2([$e|Tail]) -> [".0e"|Tail];
tofloatexp2([$E|Tail]) -> [".0e"|Tail];
tofloatexp2([Num|Tail]) -> [Num|tofloatexp(Tail)].
