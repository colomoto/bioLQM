grammar Boolsim;

// a model can start with comments and contain empty lines anywhere
model: NEWLINE* assign+ ;
assign: expr op var NEWLINE* ;

expr:  expr AND expr       #andExpr
    |  not* var            #simpleExpr
;

op: POSITIVE | NEGATIVE;
not: NOT;
var: ID ;

// spaces and line breaks
WS : [ \t\r]+ -> skip;
NEWLINE : COMMENT? '\r'? ('\n' | EOF) ;
COMMENT: '#' ~('\r' | '\n')+;

fragment LETTER: [a-zA-Z_];
fragment ALPHA: LETTER|'_';
fragment DIGIT: [0-9];
fragment IDENT: ALPHA (ALPHA|DIGIT)* ;


// token definitions
POSITIVE: '->';
NEGATIVE: '-|';
AND: '&';
NOT: '^';
ID: IDENT ;

