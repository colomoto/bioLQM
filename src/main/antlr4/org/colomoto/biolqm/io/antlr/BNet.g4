grammar BNet;

// a model can start with comments and contain empty lines anywhere
header:     'targets, factors' NEWLINE+;
assign:     var ',' expr NEWLINE+;
model:      NEWLINE* header? assign+;

expr:       expr and expr                        # andExpr
          | expr or expr                         # orExpr
          | not* ( '(' expr ')' | var | val)     # simpleExpr
;

and: '&';
or:  '|';
not: '!';
var: ID;
val: VALUE;

// to avoid "implicit token definition in parser" errors
ID: IDENT;

WS : [ \t\r]+ -> skip;
NEWLINE : COMMENT? '\r'? ('\n' | EOF) ;
VALUE: '0' | '1';
COMMENT: '#' ~('\r' | '\n')+;

fragment LETTER: [a-zA-Z_];
fragment DIGIT: [0-9];
fragment IDENT: LETTER (LETTER|DIGIT)*;
