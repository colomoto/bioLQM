grammar BooleanNet;

model: assign+ ;

assign: var '*=' expr ;

expr:  expr AND expr                      #andExpr
    | expr OR expr                        #orExpr
    | not* ( '(' expr ')' | var | val )   #simpleExpr
;

not: NOT;
var: ID ;
val: VALUE ;

// skip spaces
WS : ( ' ' | '\t' | '\r' | '\n' )+ -> skip;

fragment LETTER: [a-zA-Z_];
fragment ALPHA: LETTER|'_';
fragment DIGIT: [0-9];
fragment IDENT: ALPHA (ALPHA|DIGIT)* ;


// token definitions
AND: 'and';
OR: 'or';
NOT: 'not';
ID: IDENT ;
VALUE: DIGIT | 'true' | 'false' ;

