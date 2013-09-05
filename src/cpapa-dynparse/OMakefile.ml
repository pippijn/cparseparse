install Library ".DEFAULT" [
  (* Target *)
  Name		"cpapa-dynparse";
  Description	"C++ Dynparse interface";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Cpapa_dynparse";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
    "ccparse";
  ];
]
