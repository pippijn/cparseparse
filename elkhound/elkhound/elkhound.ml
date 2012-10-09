open GrammarType


let print_ast topforms =
  let sexpr = GrammarAst.sexp_of_topforms topforms in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let print_grammar grammar =
  let sexpr = GrammarType.sexp_of_grammar grammar in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let main () =
  let grammars =
    List.map (
      fun file ->
        let ulexbuf = Ulexing.from_utf8_channel (open_in file) in
        let parse = MenhirLib.Convert.traditional2revised
          (fun t -> t)
          (fun _ -> Lexing.dummy_pos)
          (fun _ -> Lexing.dummy_pos)
          GrammarParser.parse
        in

        let state = GrammarLexer.default_state ulexbuf in

        file, parse (fun () -> GrammarLexer.token state)
    ) ["ccparse/gr/c++1988.gr"; "ccparse/gr/c++2011.gr"; "ccparse/gr/kandr.gr"; "ccparse/gr/gnu.gr"]
  in

  (*
  for i = 1 to 100 do
    ignore (Merge.merge grammars)
  done;
  *)

  let topforms = Merge.merge grammars in
  if false then (
    PrintAst.print (Merge.to_ast topforms);
  );
  let grammar = GrammarTreeParser.of_ast topforms in
  if false then (
    List.iter PrintGrammar.print_production grammar.productions;
  );
  let env, tables = GrammarAnalysis.run_analyses grammar in

  if Array.length Sys.argv > 1 then
    Timing.progress "emitting ML code" (EmitCode.emit_ml "ccparse/gr/cc" env grammar) tables;

  if false then (
    print_grammar grammar;
  );

  ()


let () =
  Printexc.record_backtrace true;
  Printexc.print main ()
