Nonterminals technicdeflist technicdef typeexprlist typeexpr trexpr
drawexpr drawlist drawitem metalist metadef technicbody typeresults
proplist propdef expressionlist expression arithmetic funcall
variable.

Terminals '.' '{' '}' '(' ')' ':' 'end' 'draw' '->' ',' '=' '>>' typename oper
name number.

Rootsymbol technicdeflist.


technicdeflist -> technicdef technicdeflist : ['$1'|'$2'].
technicdeflist -> technicdef : ['$1'].

technicdef -> name typeexprlist metalist '->' technicbody 'end' : {technicdef, unwrap('$1'), '$2', '$3', '$5'}.
technicdef -> name typeexprlist '->' technicbody 'end' : {technicdef, unwrap('$1'), '$2', [], '$4'}.
technicdef -> name metalist '->' technicbody 'end' : {technicdef, unwrap('$1'), [], '$2', '$3'}.
technicdef -> name '->' technicbody 'end' : {technicdef, unwrap('$1'), [], [], '$3'}.

typeexprlist -> typeexpr typeexprlist : ['$1'|'$2'].
typeexprlist -> typeexpr : ['$1'].

typeresults -> typeexpr proplist typeresults : [{'$1','$2'}|'$3'].
typeresults -> typeexpr typeresults : [{'$1',[]}|'$2'].
typeresults -> typeexpr proplist : [{'$1','$2'}].
typeresults -> typeexpr : [{'$1',[]}].

technicbody -> trexpr : '$1'.

%% {abc: <expr>, cde:qdqzd}
metalist -> metadef ',' metalist  :  ['$1'|'$3'].
metalist -> metadef  :  ['$1'].
metalist -> '(' metalist ')' :  '$2'.
metadef -> name ':' expression : {unwrap('$1'), '$3'}.

%% { a = b, c = 5/x , ... }
proplist -> propdef ',' proplist  :  ['$1'] ++ '$3'.
proplist -> propdef  :  ['$1'].
proplist -> '{' proplist '}' :  '$2'.
proplist -> '{' '}' :  [].

%% a = x/2
propdef -> name '=' expression : {unwrap('$1'), '$3'}.

%% Type(x+5) | Type() | Type(x) | t:Type() | ...
typeexpr -> typename '(' ')' : {typeexpr, unwrap('$1'), 'ANON', service} .
typeexpr -> name ':' typename '(' ')' : {typeexpr, unwrap('$3'), unwrap('$1'), service} .
typeexpr -> typename '(' expression ')' :  {typeexpr, unwrap('$1'), 'ANON', '$3'} .
typeexpr -> name ':' typename '(' expression ')' :  {typeexpr, unwrap('$3'), unwrap('$1'), '$5'} .

%% x+y | x+5 | 5/(x+(y-)) | ...
arithmetic -> expression oper expression : {unwrap('$2'),'$1','$3'}. %% prefix operator

expressionlist -> expression expressionlist : ['$1'|'$2'].
expressionlist -> expression : ['$1'].

%% Expression de base
expression -> arithmetic : '$1'.
expression -> variable : '$1'.
expression -> number : unwrap('$1').
expression -> '(' expression ')' : '$2'.
expression -> funcall : '$1'.

%% Expression devant resulter par un typeresults
trexpr -> typeresults : {return, '$1'}.
trexpr -> drawexpr : '$1'.

%% Draw : match supérieur à des paliers
drawexpr -> 'draw' expression drawlist: {'draw','$2','$3'}.

drawlist -> drawitem drawlist : ['$1'|'$2'].
drawlist -> drawitem : ['$1'].

drawitem -> '>>' expression '->' trexpr : {'$2', '$4'}.
drawitem -> '>>' trexpr : {'_','$2'}.

funcall -> '(' name expressionlist ')': {'call', unwrap('$2'), '$3'}.
funcall -> '(' oper expressionlist ')': {'call', unwrap('$2'), '$3'}.
funcall ->  variable '.' name : {'call', 'getprop', unwrap('$3'), '$1'}.

variable -> name : {var, unwrap('$1')}.

Erlang code.
unwrap({_,_,V}) -> V.
