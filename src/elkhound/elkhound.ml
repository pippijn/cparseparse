open Gramanl

let (|>) = BatPervasives.(|>)


let print_ast topforms =
  let sexpr = GrammarAst.sexp_of_topforms topforms in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let print_grammar grammar =
  let sexpr = GrammarType.sexp_of_grammar grammar in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let parse files =
  List.map (fun file ->
    let ulexbuf = Ulexing.from_utf8_channel (open_in file) in
    let parse = MenhirLib.Convert.traditional2revised
      (fun t -> t)
      (fun _ -> Lexing.dummy_pos)
      (fun _ -> Lexing.dummy_pos)
      GrammarParser.parse
    in

    let state = GrammarLexer.default_state ulexbuf in

    try
      file, parse (fun () -> GrammarLexer.token state)
    with e ->
      Printf.printf "near position %d (\"%s\")\n"
        (Ulexing.lexeme_start ulexbuf)
        (Ulexing.utf8_lexeme ulexbuf);
      raise e
  ) files


let merge grammars =
  let topforms = Merge.merge grammars in
  if Options._print_merged () then
    PrintAst.print (Merge.to_ast topforms);
  topforms


let tree_parse topforms =
  let open GrammarType in

  let grammar = GrammarTreeParser.of_ast topforms in
  if false then
    List.iter PrintGrammar.print_production grammar.productions;
  if false then
    print_grammar grammar;
  grammar


let grammar_graph grammar =
  Timing.progress "writing grammar graph" GrammarGraph.visualise grammar;
  grammar


let analyse grammar =
  GrammarAnalysis.run_analyses grammar


let output_menhir (env, _, _ as tuple) =
  OutputMenhir.output_grammar env;
  tuple


let state_graph (_, states, _ as tuple) =
  Timing.progress "writing automaton graph" StateGraph.visualise states;
  tuple


let dump_automaton (env, states, _ as tuple) =
  Timing.progress "dumping states to automaton.out"
    (List.iter (PrintAnalysisEnv.print_item_set ~out:(Pervasives.open_out "automaton.out") env)) states;
  tuple


let emit_code (env, states, tables) =
  let open AnalysisEnvType in

  let terms = env.indexed_terms in
  let nonterms = env.indexed_nonterms in
  let prods_by_lhs = env.prods_by_lhs in
  let verbatims = env.verbatims in
  let impl_verbatims = env.impl_verbatims in

  Timing.progress "emitting ML code"
    (EmitCode.emit_ml "src/ccparse/gr/cc" terms nonterms prods_by_lhs verbatims impl_verbatims) tables


let optional enabled f x = if enabled () then f x else x

let main inputs =
  try
    inputs
    |> parse
    |> merge
    |> tree_parse
    |> optional Options._graph_grammar grammar_graph
    |> analyse
    |> optional Options._output_menhir output_menhir
    |> optional Options._graph_automaton state_graph
    |> optional Options._dump_automaton dump_automaton
    |> emit_code
  with Diagnostics.Diagnostic (severity, msg) ->
    Printf.printf "%s: %s\n" (Diagnostics.string_of_severity severity) msg;
    exit 1


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
