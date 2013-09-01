install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa-dyp";

  (* Sources *)
  Modules [
    "Cc";
    "Cc_keywords";
    "Cc_lexer";
    "Cc_parser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "dyp";
  ];

  Var ("NO_ANNOT", "true");
]
