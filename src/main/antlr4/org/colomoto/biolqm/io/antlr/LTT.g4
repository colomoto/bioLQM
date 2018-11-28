grammar LTT;

model:   NEWLINE* table+;
table:   curvar ('[' max ']')? SEP varid* NEWLINE+ line*;
line:    target SEP ttline NEWLINE+;
ttline:  (VALUE | JOKER)*;
curvar:  varid;
varid:   ID;
target:  VALUE;
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
