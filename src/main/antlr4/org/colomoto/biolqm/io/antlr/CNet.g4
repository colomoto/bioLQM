grammar CNet;

// a model can start with comments and contain empty lines anywhere
model:   NEWLINE* '.v' count NEWLINE+ table+;
table:   '.n' curvar count varid* NEWLINE+ line*;
line:    ttline NEWLINE+;
ttline: (INTEGER | '-')+;

curvar:  varid;
varid:   INTEGER;
count:   INTEGER;

INTEGER: DIGIT+;
NEWLINE : '\r'? ('\n' | EOF) ;
COMMENT: '#' ~('\r' | '\n')* NEWLINE -> channel(2);
WS : [ \t\r]+ -> channel(HIDDEN);

fragment DIGIT: [0-9];
