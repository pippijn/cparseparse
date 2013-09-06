#define YYDEBUG 1

#include "parser.h"

void yyerror (YYLTYPE *loc, char const *msg);
int yylex (YYSTYPE *yylval, YYLTYPE *yylloc);

extern int yydebug;
