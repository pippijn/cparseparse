install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa";

  (* Sources *)
  Modules [
    "Cpapa";
    "Options";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "ccparse";
    "glr";
  ];
]
