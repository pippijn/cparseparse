let printsexp grammar =
  let sexpr = Gramast.sexp_of_topforms grammar in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let () =
  let basedir = ".." in

  let grammars =
    List.map (
      fun file ->
        let lexbuf = Lexing.from_channel (open_in (basedir ^ "/" ^ file)) in

        file, Grampar.parse Gramlex.(token (default_state basedir lexbuf)) lexbuf
    ) ["gr/cc.gr"; "gr/gnu.gr"; "gr/kandr.gr"]
  in

  for i = 1 to 10000 do
    ignore (Merge.merge grammars)
  done;

  let merged = Merge.merge grammars in
  Printast.print merged;
