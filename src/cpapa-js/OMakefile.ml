build Program ".DEFAULT" [
  (* Target *)
  Name		"cpapa_js";

  (* Sources *)
  Modules [
    "Cpapa_js";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cpapa-dynparse";
    "glr";
    "js_intf";
  ];

  (* Only byte-code *)
  Var ("OCAML_BYTE", "true");
  Var ("OCAML_NATIVE", "false");

  Rule ("upload", "$(Name).js", [
    "chmod 644 $^";
    "scp $^ $'ra:public_html/files/up/parser/'";
  ]);
]
