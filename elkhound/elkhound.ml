let printsexp grammar =
  let sexpr = Gramast.sexp_of_topforms grammar in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let () =
  let basedir = "ccparse" in

  let grammars =
    List.map (
      fun file ->
        let ulexbuf = Ulexing.from_utf8_channel (open_in (basedir ^ "/" ^ file)) in
        let parse = MenhirLib.Convert.traditional2revised
          (fun t -> t)
          (fun _ -> Lexing.dummy_pos)
          (fun _ -> Lexing.dummy_pos)
          Grampar.parse
        in

        let state = Gramlex.default_state basedir ulexbuf in

        file, parse (fun () -> Gramlex.token state)
    ) ["gr/cc.gr"; "gr/gnu.gr"; "gr/kandr.gr"]
  in

  (*
  for i = 1 to 100 do
    ignore (Merge.merge grammars)
  done;
  *)

  let merged = Merge.merge grammars in
  Printast.print merged;
