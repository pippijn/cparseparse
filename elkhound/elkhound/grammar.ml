open Batteries_uni
open Gramast
open GrammarType
open Merge


let name_of_terminal { tbase = { name }; alias } =
  if alias <> "" then
    alias
  else
    name


let name_of_nonterminal { nbase = { name } } =
  name


let name_of_symbol = function
  | Nonterminal (_, nonterm) -> name_of_nonterminal nonterm
  | Terminal (_, term) -> name_of_terminal term


(* symbol equality ignores tags *)
let equal_symbol a b =
  match a, b with
  | Terminal (_, term_a), Terminal (_, term_b) ->
      term_a == term_b
  | Nonterminal (_, nonterm_a), Nonterminal (_, nonterm_b) ->
      nonterm_a == nonterm_b
  | _ ->
      (* terminals never equal non-terminals *)
      false


let compare_symbol a b =
  match a, b with
  (* any state with no incoming arcs (start state) is first *)
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0

  (* terminals come before nonterminals *)
  | Some (Nonterminal _), Some (Terminal _) -> 1
  | Some (Terminal _), Some (Nonterminal _) -> -1

  (* order by id within terms/nonterms *)
  | Some (Terminal (_, term_a)), Some (Terminal (_, term_b)) ->
      term_a.term_index - term_b.term_index
  | Some (Nonterminal (_, nonterm_a)), Some (Nonterminal (_, nonterm_b)) ->
      nonterm_a.nt_index - nonterm_b.nt_index


let start_name = "__EarlyStartSymbol"


(* synthesize a rule "__EarlyStartSymbol -> Start EOF" *)
let synthesise_start_rule topforms =
  (* find the name of the user's EOF token *)
  let TermDecl (_, eof, _) = List.find (fun (TermDecl (code, _, _)) -> code = 0) topforms.decls in

  (* build a start production *)
  let start =
    TF_nonterm ((* name = *)start_name, (* type = *)"", (* funcs = *)[], (* prods = *)[
      ProdDecl (PDK_NEW, [
        RH_name ("top", topforms.first_nonterm);
        RH_name ("", eof);
      ], (* code: *)"")
    ], (* subsets: *)[])
  in

  { topforms with
    nonterms = Stringmap.add start_name (start, 1) topforms.nonterms
  }


(* handle TF_option *)
let collect_options options config =
  List.fold_left (fun config -> function
    | TF_option      ("useGCDefaults",              (0 | 1 as value)) ->
        { config with useGCDefaults              = value = 1 }
    | TF_option      ("defaultMergeAborts",         (0 | 1 as value)) ->
        { config with defaultMergeAborts         = value = 1 }

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
        (verbatim, code :: impl_verbatim)
    | TF_verbatim (false, code) ->
        (code :: verbatim, impl_verbatim)

    | _ -> failwith "merge failed"
  ) ([], []) verbatims


let collect_terminal_aliases decls =
  List.fold_left (fun aliases (TermDecl (_, name, alias)) ->
    if alias <> "" then
      Stringmap.add alias name aliases
    else
      aliases
  ) Stringmap.empty decls


(* type annotations *)
let collect_terminal_types types =
  let types =
    List.fold_left (fun types (TermType (name, _, _) as termtype) ->
      if Stringmap.mem name types then
        failwith "this token already has a type";
      Stringmap.add name termtype types
    ) Stringmap.empty types
  in

  types


(* precedence specifications *)
let collect_terminal_precs precs aliases =
  let precs =
    List.fold_left (fun precs (PrecSpec (kind, prec, tokens) as termtype) ->
      List.fold_left (fun precs token ->
        let token =
          try
            Stringmap.find token aliases
          with Not_found ->
            token
        in

        if prec = 0 then
          (* 0 means precedence isn't specified *)
          failwith "you can't use 0 as a precedence level, because that value is used internally to mean something else";

        if Stringmap.mem token precs then
          failwith "this token already has a specified precedence";
        Stringmap.add token termtype precs
      ) precs tokens
    ) Stringmap.empty precs
  in

  precs


let spec_func funcs name formal_count =
  try
    let SpecFunc (_, params, code) =
      List.find (fun (SpecFunc (fname, _, _)) -> fname = name) funcs
    in

    if not (List.memq (List.length params) formal_count) then
      failwith ("incorrect number of formal parameters for '" ^ name ^ "' function");

    Some { params; code; }

  with Not_found ->
    None


let collect_terminals decls types precs =
  let max_code, terminals =
    List.fold_left (fun (max_code, terminals) (TermDecl (code, name, alias)) ->
      if Stringmap.mem name terminals then
        failwith "token already declared";

      (* annotate with declared type *)
      let semtype, funcs =
        try
          let (TermType (_, termtype, funcs)) = Stringmap.find name types in
          termtype, funcs
        with Not_found ->
          "", []
      in

      (* apply precedence spec *)
      let associativity, precedence =
        try
          let PrecSpec (kind, prec, _) = Stringmap.find name precs in
          kind, prec
        with Not_found ->
          Assoc.AK_NONASSOC, 0
      in

      let terminal = {
        tbase = {
          name;
          semtype;
          dup = spec_func funcs "dup" [1];
          (* not specified is ok, since it means the 'del' function
           * doesn't use its parameter *)
          del = spec_func funcs "del" [0; 1];
          reachable = false;
        };
        alias;
        precedence;
        associativity;
        classify = spec_func funcs "classify" [1];
        term_index = code;
      } in

      let max_code = max max_code code in

      max_code, Stringmap.add name terminal terminals
    ) (0, Stringmap.empty) decls
  in

  (* track what terminals have codes *)
  let has_code = BitSet.create (max_code + 1) in
  List.iter (fun (TermDecl (code, _, _)) ->
    BitSet.set has_code code;
  ) decls;

  let terminals =
    (* fill in any gaps in the code space; this is required because
     * later analyses assume the terminal code space is dense *)
    Enum.fold (fun terminals i ->
      if BitSet.is_set has_code i then
        terminals
      else
        let dummy_name = "__dummy_filler_token" ^ string_of_int i in
        let dummy = { empty_terminal with
          tbase = { empty_symbol_base with
            name = dummy_name;
            reachable = true;
          }
        } in
        Stringmap.add dummy_name dummy terminals
    ) terminals (Enum.seq 1 ((+) 1) ((>=) max_code))
  in

  terminals


let collect_nonterminals nonterms term_count =
  (*Sexplib.Sexp.output_hum Pervasives.stdout (Stringmap.sexp_of_t Gramast.sexp_of_topform nonterms);*)
  (*print_newline ();*)

  let nonterminals =
    Stringmap.fold (fun _ (nterm, nt_index) nonterminals ->
      match nterm with
      | TF_nonterm (name, semtype, funcs, prods, subsets) ->
          (* record subsets *)
          List.iter (fun subset ->
            if not (Stringmap.mem subset nonterms) then
              failwith "subsets contains non-existent nonterminal"
            (* note that, since context-free language inclusion is
             * undecidable (Hopcroft/Ullman), we can't actually check that
             * the given nonterminals really are in the subset relation *)
          ) subsets;

          (* make the Grammar object to represent the new nonterminal *)
          let nonterminal = { empty_nonterminal with
            nbase = {
              name;
              semtype;
              dup = spec_func funcs "dup" [1];
              del = spec_func funcs "del" [0; 1];
              reachable = false;
            };
            merge = spec_func funcs "merge" [2];
            keep  = spec_func funcs "keep"  [1];
            maximal = (match spec_func funcs "maximal" [0] with None -> false | Some _ -> true);
            (* we simply store the (validated) string references here, because
             * it is very hard to have cyclic immutable data structures *)
            subset_names = subsets;

            (* Each nonterminal needs its own first/follow sets. *)
            first  = TerminalSet.create term_count;
            follow = TerminalSet.create term_count;

            nt_index;
          } in

          Stringmap.add name nonterminal nonterminals

      | _ -> failwith "merge failed"
    ) nonterms Stringmap.empty
  in

  assert (Stringmap.cardinal nonterminals = Stringmap.cardinal nonterms);

  nonterminals


let add_forbid forbid tok =
  (* XXX: in-place update *)
  TerminalSet.set forbid tok.term_index;
  forbid


let collect_production_rhs aliases terminals nonterminals is_synthesised rhs_list production =
  let find_nonterminal name =
    Stringmap.find name nonterminals
  in

  let find_terminal name =
    let terminal =
      try
        Stringmap.find name terminals
      with Not_found ->
        let name = Stringmap.find name aliases in
        Stringmap.find name terminals
    in
    if terminal.term_index = 0 && not is_synthesised then
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
              try
                let terminal = find_terminal name in
                (* whenever we see a terminal, copy its precedence spec to
                 * the production; thus, the last symbol appearing in the
                 * production will be the one that gives the precedence *)
                Terminal (tag, terminal), terminal.precedence
              with Not_found ->
                Nonterminal (tag, find_nonterminal name), production.prec
            in

            (* add it to the production *)
            { production with right = symbol :: production.right; prec }

      | RH_string (tag, str) ->
          let term = find_terminal str in
          { production with right = Terminal (tag, term) :: production.right; prec = term.precedence }

      | RH_prec (tokName) ->
          let { precedence } = find_terminal tokName in

          (* apply the specified precedence *)
          { production with prec = precedence }

      | RH_forbid (tokName) ->
          let tok = find_terminal tokName in

          let forbid =
            add_forbid production.forbid tok
          in

          { production with forbid }

    ) production rhs_list
  in

  (* The list was built in reverse order; reverse it again, here *)
  { production with right = List.rev production.right }


let collect_productions aliases terminals nonterminals nonterms =
  let term_count = Stringmap.cardinal terminals in

  let productions, last_prod_index =
    Stringmap.fold (fun _ (nterm, _) (productions, next_prod_index) ->
      match nterm with
      | TF_nonterm (name, _, _, prods, _) ->
          let left = Stringmap.find name nonterminals in
          (* is this the special start symbol I inserted? *)
          let is_synthesised = name = start_name in

          List.fold_left (fun (productions, next_prod_index) (ProdDecl (kind, rhs, action)) ->
            (* build a production *)
            let production =
              { empty_production with
                left;
                action;
                first_rhs = TerminalSet.create term_count;
                prod_index = next_prod_index;
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

  let aliases = collect_terminal_aliases topforms.decls in
  let types = collect_terminal_types topforms.types in
  let precs = collect_terminal_precs topforms.precs aliases in

  (* process all (non)terminal declarations first, so while we're 
   * looking at productions we can tell if one isn't declared *)
  let terminals               = collect_terminals topforms.decls types precs in
  let verbatim, impl_verbatim = collect_verbatims topforms.verbatims in
  let nonterminals            = collect_nonterminals topforms.nonterms (Stringmap.cardinal terminals) in

  (* process nonterminal bodies *)
  let productions             = collect_productions aliases terminals nonterminals topforms.nonterms in
  let start_symbol            = Stringmap.find topforms.first_nonterm nonterminals in

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

  if Config.trace_merge then (
    Printf.printf "%d terminals\n" (Stringmap.cardinal terminals);
    Printf.printf "%d nonterminals\n" (Stringmap.cardinal nonterminals);
    Printf.printf "%d productions\n" (List.length productions);
  );

  grammar