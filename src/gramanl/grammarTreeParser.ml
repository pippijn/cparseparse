open GrammarAst
open GrammarType
open Merge
open Camlp4.PreCast

let (|>) = BatPervasives.(|>)
let (--) = BatPervasives.(--)

let _loc = Loc.ghost



let start_name = "__EarlyStartSymbol"


(* synthesize a rule "__EarlyStartSymbol -> Start EOF" *)
let synthesise_start_rule topforms =
  (* find the name of the user's EOF token *)
  let TermDecl (_, eof, _) = List.find (fun (TermDecl (code, _, _)) -> code = 0) topforms.decls in

  (* build a start production *)
  let start =
    TF_nonterm ((* name = *)start_name, (* type = *)None, (* funcs = *)[], (* prods = *)[
      ProdDecl (PDK_NEW, None, [
        RH_name ("top", topforms.first_nonterm);
        RH_name ("", eof);
      ], (* code: *)None)
    ], (* subsets: *)[])
  in

  { topforms with
    nonterms = StringMap.add start_name (start, Ids.Nonterminal.start) topforms.nonterms
  }


(* handle TF_option *)
let collect_options options config =
  List.fold_left (fun config -> function
    | TF_option ("shift_reduce_conflicts",   value) ->
        { config with expectedSR          = value }
    | TF_option ("reduce_reduce_conflicts",  value) ->
        { config with expectedRR          = value }
    | TF_option ("unreachable_nonterminals", value) ->
        { config with expectedUNRNonterms = value }
    | TF_option ("unreachable_terminals",    value) ->
        { config with expectedUNRTerms    = value }

    | _ -> failwith "merge failed"
  ) config options


let collect_verbatims verbatims =
  List.fold_left (fun (verbatim, impl_verbatim) -> function
    | TF_verbatim (true, code) ->
        let code =
          CamlAst.str_items_of_string _loc code
        in
        (verbatim, code :: impl_verbatim)
    | TF_verbatim (false, code) ->
        let code =
          CamlAst.sig_items_of_string _loc code
        in
        (code :: verbatim, impl_verbatim)

    | _ -> failwith "merge failed"
  ) ([], []) verbatims


let collect_terminal_aliases decls =
  List.fold_left (fun aliases (TermDecl (_, name, alias)) ->
    if alias <> "" then
      StringMap.add alias name aliases
    else
      aliases
  ) StringMap.empty decls


(* type annotations *)
let collect_terminal_types types =
  let types =
    List.fold_left (fun types (TermType (name, _, _) as termtype) ->
      if StringMap.mem name types then
        failwith "this token already has a type";
      StringMap.add name termtype types
    ) StringMap.empty types
  in

  types


(* precedence specifications *)
let collect_terminal_precs precs aliases =
  let precs =
    List.fold_left (fun precs (PrecSpec (kind, prec, tokens) as termtype) ->
      List.fold_left (fun precs token ->
        let token =
          try
            StringMap.find token aliases
          with Not_found ->
            token
        in

        if prec = 0 then
          (* 0 means precedence isn't specified *)
          failwith "you can't use 0 as a precedence level, because that value is used internally to mean something else";

        if StringMap.mem token precs then
          failwith "this token already has a specified precedence";
        StringMap.add token termtype precs
      ) precs tokens
    ) StringMap.empty precs
  in

  precs


let spec_func funcs name formal_count =
  try
    let SpecFunc (_, params, code) =
      List.find (fun (SpecFunc (fname, _, _)) -> fname = name) funcs
    in

    if List.length params <> formal_count then
      failwith ("incorrect number of formal parameters for '" ^ name ^ "' function");

    let code = CamlAst.expr_of_string _loc code in

    Some { params; code; }

  with Not_found ->
    None


let collect_terminals decls types precs =
  let max_index, terminals =
    List.fold_left (fun (max_index, terminals) (TermDecl (code, name, alias)) ->
      if StringMap.mem name terminals then
        failwith "token already declared";

      (* annotate with declared type *)
      let semtype, funcs =
        try
          let (TermType (_, termtype, funcs)) = StringMap.find name types in
          Some (CamlAst.ctyp_of_string _loc termtype), funcs
        with Not_found ->
          None, []
      in

      (* apply precedence spec *)
      let associativity, precedence =
        try
          let PrecSpec (kind, prec, _) = StringMap.find name precs in
          kind, prec
        with Not_found ->
          Assoc.AK_NONASSOC, 0
      in

      let term_index = Ids.Terminal.of_int code in

      let terminal = {
        tbase = {
          name;
          semtype;
          dup = spec_func funcs "dup" 1;
          del = spec_func funcs "del" 1;
        };
        alias;
        precedence;
        associativity;
        term_index;
        classify = spec_func funcs "classify" 1;
      } in

      let max_index = max max_index term_index in

      max_index, StringMap.add name terminal terminals
    ) (Ids.Terminal.default, StringMap.empty) decls
  in

  (* track what terminals have codes *)
  let module TerminalSet = BitSet.Make(Ids.Terminal) in
  let has_code = TerminalSet.create max_index in
  List.iter (fun (TermDecl (code, _, _)) ->
    let code = Ids.Terminal.of_int code in
    TerminalSet.add has_code code;
  ) decls;

  let terminals =
    (* fill in any gaps in the code space; this is required because
     * later analyses assume the terminal code space is dense *)
    Ids.Terminal.fold_left (fun terminals i ->
      if TerminalSet.mem has_code i then
        terminals
      else
        let dummy_name = "Dummy_filler_token" ^ Ids.Terminal.to_string i in
        let dummy = { empty_terminal with
          tbase = { empty_symbol_base with
            name = dummy_name;
          };
          term_index = i;
        } in
        StringMap.add dummy_name dummy terminals
    ) terminals max_index
  in

  terminals


let collect_nonterminals nonterms term_count =
  (*Sexplib.Sexp.output_hum stdout (StringMap.sexp_of_t Gramast.sexp_of_topform nonterms);*)
  (*print_newline ();*)

  let nonterminals =
    StringMap.fold (fun _ (nterm, nt_index) nonterminals ->
      match nterm with
      | TF_nonterm (name, semtype, funcs, prods, subsets) ->
          (* record subsets *)
          List.iter (fun subset ->
            if not (StringMap.mem subset nonterms) then
              failwith "subsets contains non-existent nonterminal"
            (* note that, since context-free language inclusion is
             * undecidable (Hopcroft/Ullman), we can't actually check that
             * the given nonterminals really are in the subset relation *)
          ) subsets;

          (* make the Grammar object to represent the new nonterminal *)
          let nonterminal = { empty_nonterminal with
            nbase = {
              name;
              semtype = BatOption.map (CamlAst.ctyp_of_string _loc) semtype;
              dup = spec_func funcs "dup" 1;
              del = spec_func funcs "del" 1;
            };
            merge = spec_func funcs "merge" 2;
            keep  = spec_func funcs "keep"  1;
            maximal = (match spec_func funcs "maximal" 0 with None -> false | Some _ -> true);
            (* we simply store the (validated) string references here, because
             * it is very hard to have cyclic immutable data structures *)
            subset_names = subsets;

            nt_index;
          } in

          StringMap.add name nonterminal nonterminals

      | _ -> failwith "merge failed"
    ) nonterms StringMap.empty
  in

  assert (StringMap.cardinal nonterminals = StringMap.cardinal nonterms);

  nonterminals


let collect_production_rhs aliases terminals nonterminals is_synthesised rhs_list production =
  let find_nonterminal name =
    try
      StringMap.find name nonterminals
    with Not_found ->
      failwith ("no symbol found named " ^ name)
  in

  let find_terminal name =
    let terminal =
      try
        StringMap.find name terminals
      with Not_found ->
        let name =
          try
            StringMap.find name aliases
          with Not_found ->
            failwith ("terminal \"" ^ name ^ "\" must be defined")
        in
        StringMap.find name terminals
    in

    if Ids.Terminal.is_eof terminal.term_index && not is_synthesised then
      failwith "you cannot use the EOF token in your rules";
    terminal
  in

  let production =
    List.fold_left (fun production -> function
      | RH_name (tag, name) ->
          (* "empty" is a syntactic convenience; it doesn't get
           * added to the production *)
          if name = empty_nonterminal.nbase.name then
            production
          else
            let symbol, prec =
              try (* look up terminal *)
                let terminal = find_terminal name in
                (* whenever we see a terminal, copy its precedence spec to
                 * the production; thus, the last symbol appearing in the
                 * production will be the one that gives the precedence *)
                Terminal (tag, terminal.term_index), terminal.precedence
              with Failure _ ->
                let nonterminal = find_nonterminal name in
                (* keep old precedence *)
                Nonterminal (tag, nonterminal.nt_index), production.prec
            in

            (* add it to the production *)
            { production with right = symbol :: production.right; prec }

      | RH_string (tag, str) ->
          let term = find_terminal str in
          { production with right = Terminal (tag, term.term_index) :: production.right; prec = term.precedence }

      | RH_prec (tokName) ->
          let { precedence } = find_terminal tokName in

          (* apply the specified precedence *)
          { production with prec = precedence }

      | RH_forbid (tokName) ->
          let tok = find_terminal tokName in

          let forbid =
            TerminalSet.add tok.term_index production.forbid
          in

          { production with forbid }

    ) production rhs_list
  in

  (* The list was built in reverse order; reverse it again, here *)
  { production with right = List.rev production.right }


let collect_productions aliases terminals nonterminals nonterms =
  let productions, last_prod_index =
    StringMap.fold (fun _ (nterm, _) (productions, next_prod_index) ->
      match nterm with
      | TF_nonterm (name, _, _, prods, _) ->
          let left = (StringMap.find name nonterminals).nt_index in
          (* is this the special start symbol I inserted? *)
          let is_synthesised = name = start_name in

          List.fold_left (fun (productions, next_prod_index) (ProdDecl (kind, prod_name, rhs, action)) ->
            let action =
              BatOption.map (CamlAst.expr_of_string _loc) action
            in

            (* build a production *)
            let production =
              { empty_production with
                left;
                action;
                prod_name;
                prod_index = Ids.Production.of_int next_prod_index;
              }
              (* deal with RHS elements *)
              |> collect_production_rhs aliases terminals nonterminals is_synthesised rhs
            in

            (* add production to grammar *)
            production :: productions, next_prod_index + 1
          ) (productions, next_prod_index) prods

      | _ -> failwith "merge failed"
    ) nonterms ([], 0)
  in

  assert (last_prod_index = List.length productions);
  productions


let of_ast topforms =
  let topforms = synthesise_start_rule topforms in

  if Options._print_merged () then
    PrintAst.print (Merge.to_ast topforms);

  let aliases = collect_terminal_aliases topforms.decls in
  let types = collect_terminal_types topforms.types in
  let precs = collect_terminal_precs topforms.precs aliases in

  (* process all (non)terminal declarations first, so while we're 
   * looking at productions we can tell if one isn't declared *)
  let terminals               = collect_terminals topforms.decls types precs in
  let verbatim, impl_verbatim = collect_verbatims topforms.verbatims in
  let nonterminals            = collect_nonterminals topforms.nonterms (StringMap.cardinal terminals) in

  (* process nonterminal bodies *)
  let productions             = collect_productions aliases terminals nonterminals topforms.nonterms in
  let start_symbol            = topforms.first_nonterm in

  let config                  = collect_options topforms.options empty_config in

  let grammar = {
    nonterminals;
    terminals;
    aliases;
    productions;
    start_symbol;

    verbatim;
    impl_verbatim;

    config;
  } in

  if Options._trace_merge () then (
    Printf.printf "%d terminals\n" (StringMap.cardinal terminals);
    Printf.printf "%d nonterminals\n" (StringMap.cardinal nonterminals);
    Printf.printf "%d productions\n" (List.length productions);
  );

  grammar
