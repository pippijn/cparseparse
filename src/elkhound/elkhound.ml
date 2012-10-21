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


let dirname s =
  let point = String.rindex s '/' in
  String.sub s 0 point


let parse files =
  dirname (List.hd files),
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


let merge (dirname, grammars) =
  let topforms = Merge.merge grammars in
  if Options._print_merged () then
    PrintAst.print (Merge.to_ast topforms);
  dirname, topforms


let tree_parse (dirname, topforms) =
  let open GrammarType in

  let grammar = GrammarTreeParser.of_ast topforms in
  if false then
    List.iter PrintGrammar.print_production grammar.productions;
  if false then
    print_grammar grammar;
  dirname, grammar


let grammar_graph (dirname, grammar) =
  let file = dirname ^ "/grammar.dot" in
  Timing.progress "writing grammar graph" (GrammarGraph.visualise ~file) grammar;
  dirname, grammar


let analyse (dirname, grammar) =
  dirname, GrammarAnalysis.run_analyses grammar


let print_transformed (dirname, (env, _, _) as tuple) =
  let file = dirname ^ "/grammar.gr" in
  let ast = BackTransform.ast_of_env env in
  BatStd.with_dispose ~dispose:close_out
    (fun out -> PrintAst.print ~out ast) (open_out file);
  tuple


let output_menhir (dirname, (env, _, _) as tuple) =
  let file = dirname ^ "/grammar.mly" in
  OutputMenhir.output_grammar ~file env;
  tuple


let state_graph (dirname, (_, states, _) as tuple) =
  let file = dirname ^ "/automaton.dot" in
  Timing.progress "writing automaton graph" (StateGraph.visualise ~file) states;
  tuple


let dump_automaton (dirname, (env, states, _) as tuple) =
  let out = Pervasives.open_out (dirname ^ "/automaton.out") in
  Timing.progress "dumping states to automaton.out"
    (List.iter (PrintAnalysisEnv.print_item_set ~out env)) states;
  close_out out;
  tuple


let emit_code (dirname, (env, states, tables)) =
  let open AnalysisEnvType in

  let index = env.index in
  let prods_by_lhs = env.prods_by_lhs in
  let verbatims = env.verbatims in
  let impl_verbatims = env.impl_verbatims in

  let reachable = Reachability.compute_reachable_tagged index.prods prods_by_lhs in

  let transform inner prefix =
    let module Transform = PtreeMaker.Make(struct
      let prefix = prefix
      let inner = inner
    end) in

    prefix,
    Transform.nonterms reachable index.nonterms,
    Transform.prods reachable prods_by_lhs index.prods
  in

  let actions = [
    (* original user actions *)
    "", index.nonterms, index.prods;
    (* transform to use "%Ptree" module *)
    transform "Ptree" "Ptree";
    (* transform to use "%Treematch" module *)
    transform "Ptree" "Treematch";
  ] in

  Timing.progress "emitting ML code"
    (EmitCode.emit_ml dirname index prods_by_lhs actions reachable verbatims impl_verbatims) tables


let optional enabled f x = if enabled () then f x else x

let main inputs =
  try
    inputs
    |> parse
    |> merge
    |> tree_parse
    |> optional Options._graph_grammar grammar_graph
    |> analyse
    |> optional Options._print_transformed print_transformed
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
