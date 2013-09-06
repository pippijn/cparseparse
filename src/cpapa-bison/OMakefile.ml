install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa-bison";

  Sources [
    "lexer.l";
    "main.c";
    "parser.y";
  ];

  Headers [
    "main.h";
    "parser.h";
  ];

  Var ("OM_CFLAGS", "-O2");
  Var ("OM_YFLAGS", "-d");
]
