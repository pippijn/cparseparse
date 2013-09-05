install Library ".DEFAULT" [
  (* Target *)
  Name		"ccparse";
  Description	"C++ Parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "CcLexer";
    "CcTerminals";
    "Cc_keywords";
    "Factory";
    "Options";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "ccabs";
  ];

  Grammars [
    "gr/c++1998.gr";
    "gr/c++2011.gr";
    "gr/gnu.gr";
    "gr/kandr.gr";
  ];

  Tokens [
    "tok/c++1998.tok";
    "tok/c++2011.tok";
    "tok/gnu.tok";
  ];

  Parser (Elkhound, "cc");
]
