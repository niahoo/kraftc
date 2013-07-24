Nonterminals technicdeflist technicdef typeinputlist typeinput
typeoutput trexpr drawexpr drawlist drawitem metalist metadef
technicbody typeresults proplist propdef expressionlist expression
arithmetic funcall variable oper.

Terminals '.' '{' '}' '(' ')' ':' 'end' 'draw' '->' ',' '=' '>>' typename
name number string '+' '-' '*' '/' '_'.

Rootsymbol technicdeflist.

Left 400 '*'.
Left 400 '/'.
Left 300 '+'.
Left 300 '-'.

technicdeflist -> technicdef technicdeflist : ['$1'|'$2'].
technicdeflist -> technicdef : ['$1'].

technicdef -> name typeinputlist metalist '->' technicbody 'end' : {technicdef, unwrap_OFF('$1'), '$2', '$3', '$5'}.
technicdef -> name typeinputlist '->' technicbody 'end' : {technicdef, unwrap_OFF('$1'), '$2', [], '$4'}.
technicdef -> name metalist '->' technicbody 'end' : {technicdef, unwrap_OFF('$1'), [], '$2', '$3'}.
technicdef -> name '->' technicbody 'end' : {technicdef, unwrap_OFF('$1'), [], [], '$3'}.

typeinputlist -> typeinput typeinputlist : ['$1'|'$2'].
typeinputlist -> typeinput : ['$1'].

typeresults -> typeoutput proplist typeresults : [{'$1','$2'}|'$3'].
typeresults -> typeoutput typeresults : [{'$1',[]}|'$2'].
typeresults -> typeoutput proplist : [{'$1','$2'}].
typeresults -> typeoutput : [{'$1',[]}].

technicbody -> trexpr : '$1'.

%% Expression devant resulter par un typeresults
trexpr -> typeresults : {return, '$1'}.
trexpr -> drawexpr : '$1'.


%% {abc: <expr>, cde:qdqzd}
metalist -> metadef ',' metalist  :  ['$1'|'$3'].
metalist -> metadef  :  ['$1'].
metalist -> '(' metalist ')' :  '$2'.
metadef -> name ':' expression : {unwrap_OFF('$1'), '$3'}.

%% { a = b, c = 5/x , ... }
proplist -> propdef ',' proplist  :  ['$1'] ++ '$3'.
proplist -> propdef  :  ['$1'].
proplist -> '{' proplist '}' :  '$2'.
proplist -> '{' '}' :  [].

%% a = x/2
propdef -> name '=' expression : {unwrap_OFF('$1'), '$3'}.

%% Type(10) | Type() | t:Type(5) | ...
typeinput -> typename '(' ')' : {typeinput, unwrap_OFF('$1'), 'ANON', service} .
typeinput -> name ':' typename '(' ')' : {typeinput, unwrap_OFF('$3'), unwrap_OFF('$1'), service} .
typeinput -> typename '(' number ')' :  {typeinput, unwrap_OFF('$1'), 'ANON', '$3'} .
typeinput -> name ':' typename '(' number ')' :  {typeinput, unwrap_OFF('$3'), unwrap_OFF('$1'), '$5'} .

%% Type(x+5) | Type() | Type(x) | t:Type() | ...
typeoutput -> typename '(' ')' : {typeoutput, unwrap_OFF('$1'), 'ANON', service} .
typeoutput -> name ':' typename '(' ')' : {typeoutput, unwrap_OFF('$3'), unwrap_OFF('$1'), service} .
typeoutput -> typename '(' expression ')' :  {typeoutput, unwrap_OFF('$1'), 'ANON', '$3'} .
typeoutput -> name ':' typename '(' expression ')' :  {typeoutput, unwrap_OFF('$3'), unwrap_OFF('$1'), '$5'} .


expressionlist -> expression expressionlist : ['$1'|'$2'].
expressionlist -> expression : ['$1'].

%% Expression de base
expression -> arithmetic : '$1'.
expression -> variable : '$1'.
expression -> number : unwrap_OFF('$1').
expression -> '(' expression ')' : '$2'.
expression -> funcall : '$1'.
expression -> string : '$1'.


%% Draw : match supérieur à des paliers
drawexpr -> 'draw' expression drawlist 'end': {'draw','$2','$3'}.

drawlist -> drawitem drawlist : ['$1'|'$2'].
drawlist -> drawitem : ['$1'].

drawitem -> '>>' number '->' trexpr : {'$2','$4'}.
drawitem -> '>>' '_' '->' trexpr : {'$2','$4'}.

funcall -> '(' name expressionlist ')': {'call', unwrap_OFF('$2'), '$3'}.
funcall -> '(' oper expressionlist ')': {'call', unwrap_OFF('$2'), '$3'}.
funcall ->  variable '.' name : {'call', 'getprop', [chgatom('$3',key), '$1']}.

variable -> name : chgatom('$1', 'var').

oper -> '+' : unwrap('$1').
oper -> '-' : unwrap('$1').
oper -> '*' : unwrap('$1').
oper -> '/' : unwrap('$1').

%% x+y | x+5 | 5/(x+(y-)) | ...

arithmetic -> expression '+' expression : {unwrap('$2'),'$1','$3'}.
arithmetic -> expression '-' expression : {unwrap('$2'),'$1','$3'}.
arithmetic -> expression '/' expression : {unwrap('$2'),'$1','$3'}.
arithmetic -> expression '*' expression : {unwrap('$2'),'$1','$3'}.


Erlang code.
unwrap({V,_}) -> V;
unwrap({_,_,V}) -> V.

unwrap_OFF(X) -> X.

%% Remplace l'atom en tête de tuple
chgatom({_,L,V}, Atom) -> {Atom,L,V}.

