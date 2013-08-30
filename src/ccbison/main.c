#include <stdio.h>
#include <stdlib.h>

#include "main.h"

void
yyerror (YYLTYPE *loc, char const *msg)
{
  printf ("<stdin>:[%d:%d]-[%d:%d]: %s\n",
          loc->first_line,
          loc->first_column,
          loc->last_line,
          loc->last_column,
          msg);
  exit (1);
}

int
main ()
{
  yydebug = 1;
  yyparse ();
}
