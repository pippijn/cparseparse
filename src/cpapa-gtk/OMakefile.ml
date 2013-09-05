install Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa_gtk";

  (* Sources *)
  Modules [
    "Cpapa_gtk";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cpapa-dynparse";
    "glr";
    "gtk_intf";
  ];
]
