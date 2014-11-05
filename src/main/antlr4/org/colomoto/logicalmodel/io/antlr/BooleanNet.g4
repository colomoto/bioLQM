grammar BooleanNet;

// a model can start with comments and contain empty lines anywhere
model: comment* assign+ ;
comment: '#' ~NEWLINE*? NEWLINE+ ;
assign: var '*' '=' expr NEWLINE* ;

expr:  expr AND expr                      #andExpr
    | expr OR expr                        #orExpr
    | not* ( '(' expr ')' | var | val )   #simpleExpr
;

not: NOT;
var: ID ;
val: VALUE ;

// spaces and line breaks
WS : [' ' '\t' '\r' ]+ -> channel(HIDDEN);
NEWLINE : '\r'? '\n' ;

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

