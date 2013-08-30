install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa";

  (* Sources *)
  Modules [
    "Cpapa";
    "Parser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "ccparse";
    "glr";
  ];
]
