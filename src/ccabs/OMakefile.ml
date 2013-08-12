install Package ".DEFAULT" [
  (* Target *)
  Name		"ccabs";
  Description	"C++ Abstract Syntax Tree";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Ast";
    "Disambiguation";
    "Flags";
    "Query";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "monad-custom";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "ast.ml",			"-syntax camlp4o";
    "disambiguation.ml",	"-syntax camlp4o";
  ];
]
