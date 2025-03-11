lexer grammar CombatScriptLexer;

//arithmetic
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';
MOD: '%';
POW: '**';

//bitwise
BWNEG: '~';
BWAND: '&';
BWOR: '|';
BWXOR: '^';
BWLS: '<<';
BWRS: '>>';

//comparisons
EQ: '==';
NEQ: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';

//Logical operators
NEG: '!';
AND: '&&';
OR: '||';

//variables
VAR: 'var';
ASSIGN: '=';

//BRANCHING
IF: 'if';
ELSE: 'else';
SWITCH: 'switch';
CASE: 'case';
DEFAULT: 'default';

//LOOPS
WHILE: 'while';
FOR: 'for';
FOREACH: 'foreach';
IN: 'in';
BREAK: 'break';
CONTINUE: 'continue';
RETURN: 'return';

//MISC
INCR: '++';
DECR: '--';
LP: '(';
RP: ')';
LB: '[';
RB: ']';
LC: '{';
RC: '}';
COLON: ':';
SEMICOLON: ';';
COMMA: ',';
RANGE: '..';
DOT: '.';

//primitives
LAMBDA: '=>';
NULL: 'null';
INT: ('+' | '-')? UINT;
FLOAT: (INT | ('+' | '-'))? .? UINT 'f';
UINT: [0-9]+;
BOOL: 'true' | 'false';
STRING: '"' (~["\\])* '"';
NEW: 'new';
ID: [a-zA-Z0-9_]+;

WS: [ \t\r\n]+ -> skip;
COMMENT: '//'(~[\n])* -> skip;