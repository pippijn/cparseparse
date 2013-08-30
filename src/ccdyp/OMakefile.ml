install Program ".DEFAULT" [
  (* Target *)
  Name		"ccdyp";

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
