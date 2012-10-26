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
    let lexbuf = Lexing.from_channel (open_in file) in
    Lexing.(lexbuf.lex_curr_p <- {
      pos_fname = file;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    });

    let parse = MenhirLib.Convert.traditional2revised
      (fun (t, s, e) -> t)
      (fun (t, s, e) -> s)
      (fun (t, s, e) -> e)
      GrammarParser.parse
    in

    let state = GrammarLexer.default_state lexbuf in

    try
      file, parse (fun () -> GrammarLexer.token state)
    with e ->
      Printf.printf "near position %d (\"%s\")\n"
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme lexbuf);
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
    print_grammar grammar;
  grammar


let init_env grammar =
  let env = AnalysisEnv.init_env grammar in
  grammar, env


let analyse (grammar, env) =
  let open GrammarType in

  let env, (states, tables) = GrammarAnalysis.run_analyses env grammar.nonterminals in
  grammar, env, states, tables


let grammar_graph dirname (grammar, env, _, _ as tuple) =
  let open AnalysisEnvType in
  let open GrammarType in

  let file = dirname ^ "/grammar.dot" in
  Timing.progress "writing grammar graph to grammar.dot"
    (GrammarGraph.visualise ~file env.index.nonterms) grammar;
  tuple


let print_transformed dirname (_, env, _, _ as tuple) =
  let open AnalysisEnvType in

  Timing.progress "writing transformed grammars to grammar.gr"
    SemanticVariant.iter (fun variant ->
      let file = dirname ^ "/grammar.gr" in
      let ast = BackTransform.ast_of_env env variant in
      BatStd.with_dispose ~dispose:close_out
        (fun out -> PrintAst.print ~out ast) (open_out file);
    );
  tuple


let output_menhir dirname (_, env, _, _ as tuple) =
  Timing.progress "writing menhir grammar to grammar.mly"
    SemanticVariant.iter (fun variant ->
      let file = dirname ^ "/grammar.mly" in
      OutputMenhir.output_grammar ~file variant env
    );
  tuple


let state_graph dirname (_, _, states, _ as tuple) =
  let file = dirname ^ "/automaton.dot" in
  Timing.progress "writing automaton graph to automaton.dot"
    (StateGraph.visualise ~file) states;
  tuple


let dump_automaton dirname (_, env, states, _ as tuple) =
  BatStd.with_dispose ~dispose:close_out
    (fun out ->
      Timing.progress "dumping states to automaton.out"
        (List.iter (PrintAnalysisEnv.print_item_set env out)) states
    ) (Pervasives.open_out (dirname ^ "/automaton.out"));
  tuple


let emit_code dirname (_, env, states, tables) =
  let open AnalysisEnvType in

  let index = env.index in
  let prods_by_lhs = env.prods_by_lhs in
  let reachable = env.reachable in
  let ptree = env.ptree in
  let verbatims = env.verbatims in

  Timing.progress "emitting ML code"
    (EmitCode.emit_ml dirname index prods_by_lhs verbatims reachable ptree) tables


let optional enabled f x = if enabled () then f x else x

let dirname s =
  let point = String.rindex s '/' in
  String.sub s 0 point


let main inputs =
  let dirname = dirname (List.hd inputs) in

  try
    inputs
    |> Timing.progress "parsing grammar files" parse
    |> Timing.progress "merging modules" merge
    |> Timing.progress "extracting grammar structure" tree_parse
    |> Timing.progress "adding parse tree actions" init_env
    |> analyse
    |> optional Options._graph_grammar (grammar_graph dirname)
    |> optional Options._print_transformed (print_transformed dirname)
    |> optional Options._output_menhir (output_menhir dirname)
    |> optional Options._graph_automaton (state_graph dirname)
    |> optional Options._dump_automaton (dump_automaton dirname)
    |> Valgrind.Callgrind.instrumented (emit_code dirname)
  with Diagnostics.Diagnostic (severity, msg) ->
    Printf.printf "%s: %s\n" (Diagnostics.string_of_severity severity) msg;
    exit 1


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
