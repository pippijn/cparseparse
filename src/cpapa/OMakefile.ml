install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa";

  (* Sources *)
  Modules [
    "Cpapa";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "ccparse";
    "glr";
  ];
]
