install Package ".DEFAULT" [
  (* Target *)
  Name		"ccparse";
  Description	"C++ Parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Cc_keywords";
    "CcTerminals";
    "Factory";
    "Lexer";
    "Options";
    "Parser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "ccabs";
  ];

  Grammars [
    "gr/c++1998.gr";
    "gr/c++2011.gr";
    "gr/kandr.gr";
    "gr/gnu.gr";
  ];

  Tokens [
    "tok/c++1998.tok";
    "tok/c++2011.tok";
    "tok/gnu.tok";
  ];

  Parser (Elkhound, "cc");
]
