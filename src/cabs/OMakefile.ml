install Package ".DEFAULT" [
  (* Target *)
  Name		"cabs";
  Description	"C Abstract Syntax Tree";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Ast";
    "Constant";
    "Sclass";
    "Tqual";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "batteries";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "ast.ml",		"-syntax camlp4o";
    "constant.ml",	"-syntax camlp4o";
    "sclass.ml",	"-syntax camlp4o";
    "tqual.ml",		"-syntax camlp4o";
  ];
]
