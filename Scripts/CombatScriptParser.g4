parser grammar CombatScriptParser;

options {
	tokenVocab = CombatScriptLexer;
}

program: scope* EOF;

expression: (LP expression RP)
	| SUB expression
	| expression (MOD) expression
	| expression (POW) expression
	| expression (MUL | DIV) expression
	| expression (ADD | SUB) expression
	| BWNEG expression
	| expression BWAND expression
	| expression BWOR expression
	| expression BWXOR expression
	| expression (BWLS | BWRS) expression
	| expression (EQ | NEQ | LT | LE | GT | GE) expression
	| NEG expression
	| expression AND expression
	| expression OR expression
	| value;

member_access: DOT ID;
function_call: LP (expression (COMMA expression)*)? RP;
array_access: LB expression (COMMA expression)* RB;
primitive: (
		ID
		| FLOAT
		| INT
		| BOOL
		| STRING
		| NULL
		| range
		| lambda
		| array
		| ctor
	);
value:
	primitive (member_access | function_call | array_access)*;
ctor: NEW ID LP (expression (COMMA expression)*)? RP;
range: INT RANGE INT;
lambda:
	LP (ID (COMMA ID)*)? RP LAMBDA (
		expression
		| (LC scope* RC)
	);
array: LB (expression (COMMA expression)+)? RB;

arithmetic_op: ADD | SUB | MUL | DIV | MOD | POW;
bitwise_op: BWAND | BWOR | BWXOR | BWLS | BWRS;
comparison_op: EQ | NEQ | LT | LE | GT | GE;
logical_op: AND | OR;

variable_definition: VAR ID (ASSIGN expression)?;
variable_access:
	ID
	| (expression (member_access | array_access));
variable_assignment:
	variable_access (arithmetic_op | bitwise_op)? ASSIGN expression;
variable_increment: (INCR | DECR) variable_access
	| variable_access (INCR | DECR);

statement: (
		variable_definition
		| variable_assignment
		| variable_increment
		| expression
		| RETURN expression?
		| BREAK
		| CONTINUE
	)?;
//scopes
scope:
	(LC scope* RC)
	| (statement SEMICOLON)
	| for_loop
	| foreach_loop
	| while_loop
	| conditional
	| switch;

//branching
conditional:
	IF LP expression RP scope (ELSE IF LP expression RP scope)* (
		ELSE scope
	)?;
switch: SWITCH LP expression RP LC switchBranch+ RC;
switchBranch: case (COMMA case)* COLON scope;
case: (expression | DEFAULT);

//loops
for_loop:
	FOR LP (statement SEMICOLON expression SEMICOLON statement) RP scope;
foreach_loop: FOREACH LP VAR ID IN expression RP scope;
while_loop: WHILE LP expression RP scope;