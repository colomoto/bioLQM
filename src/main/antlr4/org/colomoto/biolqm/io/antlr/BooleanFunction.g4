grammar BooleanFunction;

// a model can start with comments and contain empty lines anywhere
model: NEWLINE* assign+ ;
assign: var ':' expr NEWLINE+ ;

expr:  expr AND expr                      #andExpr
    | expr OR expr                        #orExpr
    | not* ( '(' expr ')' | var | val )   #simpleExpr
;

not: NOT;
var: ID ;
val: VALUE ;

// spaces and line breaks
WS : [ \t\r]+ -> skip;
NEWLINE : COMMENT? '\r'? ('\n' | EOF) ;
COMMENT: '#' ~('\r' | '\n')+;

fragment LETTER: [a-zA-Z_];
fragment ALPHA: LETTER|'_';
fragment DIGIT: [0-9];
fragment IDENT: ALPHA (ALPHA|DIGIT)* ;


// token definitions
AND: '&';
OR: '|';
NOT: '!';
ID: IDENT ;
VALUE: DIGIT;

