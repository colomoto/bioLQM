grammar MNet;

// A model is a list of assignments with empty lines anywhere
assign:     var '<-' expr NEWLINE+;
model:      NEWLINE* assign+;

expr:       expr and expr                        # andExpr
          | expr or expr                         # orExpr
          | not* ( '(' expr ')' | var | val)     # simpleExpr
;

and: '&';
or:  '|';
not: '!';
var: ID (TS VALUE)?;
val: VALUE;

// to avoid "implicit token definition in parser" errors
ID: IDENT ;
TS: TSEP;
VALUE: DIGIT;

WS : [' ' '\t' '\r']+ -> channel(HIDDEN);
NEWLINE : COMMENT? '\r'? ('\n' | EOF) ;
COMMENT: '#' ~('\r' | '\n')+;

fragment LETTER: [a-zA-Z_];
fragment DIGIT: [0-9];
fragment TSEP: ':';
fragment IDENT: LETTER (LETTER|DIGIT)*;