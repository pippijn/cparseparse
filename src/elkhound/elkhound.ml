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


let grammar_graph dirname grammar =
  let file = dirname ^ "/grammar.dot" in
  Timing.progress "writing grammar graph" (GrammarGraph.visualise ~file) grammar;
  grammar


let analyse grammar =
  let env, (states, tables) = GrammarAnalysis.run_analyses grammar in
  env, states, tables


let print_transformed dirname (env, _, _ as tuple) =
  let open AnalysisEnvType in

  List.iter (fun variant ->
    let file = dirname ^ "/grammar.gr" in
    let ast = BackTransform.ast_of_env env variant in
    BatStd.with_dispose ~dispose:close_out
      (fun out -> PrintAst.print ~out ast) (open_out file);
  ) env.variants;
  tuple


let output_menhir dirname (env, _, _ as tuple) =
  let file = dirname ^ "/grammar.mly" in
  OutputMenhir.output_grammar ~file env;
  tuple


let state_graph dirname (_, states, _ as tuple) =
  let file = dirname ^ "/automaton.dot" in
  Timing.progress "writing automaton graph" (StateGraph.visualise ~file) states;
  tuple


let dump_automaton dirname (env, states, _ as tuple) =
  let out = Pervasives.open_out (dirname ^ "/automaton.out") in
  Timing.progress "dumping states to automaton.out"
    (List.iter (PrintAnalysisEnv.print_item_set ~out env)) states;
  close_out out;
  tuple


let emit_code dirname (env, states, tables) =
  let open AnalysisEnvType in

  let index = env.index in
  let prods_by_lhs = env.prods_by_lhs in
  let variants = env.variants in

  let reachable = Reachability.compute_reachable_tagged index.prods prods_by_lhs in

  Timing.progress "emitting ML code"
    (EmitCode.emit_ml dirname index prods_by_lhs variants reachable) tables


let optional enabled f x = if enabled () then f x else x

let dirname s =
  let point = String.rindex s '/' in
  String.sub s 0 point


let main inputs =
  let dirname = dirname (List.hd inputs) in

  try
    inputs
    |> parse
    |> merge
    |> tree_parse
    |> optional Options._graph_grammar (grammar_graph dirname)
    |> analyse
    |> optional Options._print_transformed (print_transformed dirname)
    |> optional Options._output_menhir (output_menhir dirname)
    |> optional Options._graph_automaton (state_graph dirname)
    |> optional Options._dump_automaton (dump_automaton dirname)
    |> emit_code dirname
  with Diagnostics.Diagnostic (severity, msg) ->
    Printf.printf "%s: %s\n" (Diagnostics.string_of_severity severity) msg;
    exit 1


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
