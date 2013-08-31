install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa";

  (* Sources *)
  Modules [
    "Cpapa";
    "Options";
    "Parser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "ccparse";
    "glr";
  ];
]
