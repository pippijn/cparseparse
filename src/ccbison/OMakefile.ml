install Program ".DEFAULT" [
  (* Target *)
  Name		"ccbison";

  Sources [
    "lexer.l";
    "main.c";
    "parser.y";
  ];

  Headers [
    "main.h";
    "parser.h";
  ];
]
