grammar LTT;

model:   NEWLINE* table+;
table:   var* SEP curvar ('[' max ']')? NEWLINE+ line*;
line:    ttline SEP target NEWLINE+;
ttline:  (VALUE | JOKER)*;
curvar:  var;
target:  VALUE;
var:     ID;
max:     VALUE;

VALUE:   DIGIT;
NEWLINE: '\r'? ('\n' | EOF) ;
COMMENT: '#' ~('\r' | '\n')* NEWLINE -> channel(HIDDEN);
ID:      IDENT ;
WS :     [ \t\r]+ -> channel(HIDDEN);
SEP:     ':';
JOKER:   '-';

fragment DIGIT: [0-9];
fragment IDENT: LETTER (LETTER|DIGIT)*;
fragment LETTER: [a-zA-Z_];
