open GrammarType

(************************************************
 * :: Output helpers
 ************************************************)


let output_newline out =
  output_string out "\n"

let output_endline out line =
  output_string out line;
  output_newline out

let output_int out i =
  output_string out (string_of_int i)


let closing f x channels =
  let r =
    try
      f x
    with e ->
      List.iter close_out channels;
      raise e
  in
  List.iter close_out channels;
  r


(************************************************
 * :: Semantic type helpers
 ************************************************)


let semtype sym =
  if sym.semtype = "" then
    "'t" ^ sym.name
  else
    sym.semtype


let final_semtype final_prod =
  match final_prod.right with
  | Nonterminal (_, { nbase = { semtype } }) :: _ ->
      if semtype = "" then
        failwith "final nonterminal needs defined type"
      else if semtype.[0] = '\'' then
        failwith "final nonterminal cannot be polymorphic"
      else
        semtype
  | _ ->
      failwith "could not find final nonterminal"


(************************************************
 * :: Common output functions
 ************************************************)


let emit_ml_prologue out =
  (* prologue *)
  output_endline out "(* *** DO NOT EDIT BY HAND *** *)";
  output_endline out "(* automatically generated by parsgen *)";
  output_newline out


let emit_ml_user_code ?(braces=true) out code =
  if braces then
    output_string out "(";
  output_string out code;
  if braces then
    output_string out ")"


(************************************************
 * :: Tokens
 ************************************************)


let emit_ml_token_type out terms =
  output_endline out "type t =";
  Array.iter (fun term ->
    output_string out "  | ";
    output_string out term.tbase.name;
    if term.tbase.semtype <> "" then (
      output_string out " of ";
      output_string out term.tbase.semtype;
    );
    output_newline out;
  ) terms;
  output_newline out


let emit_ml_token_fn out name value terms =
  Printf.fprintf out "let %s = function\n" name;
  Array.iter (fun term ->
    output_string out "  | ";
    output_string out term.tbase.name;
    if term.tbase.semtype <> "" then (
      output_string out " _";
    );
    output_string out " -> ";
    output_string out (value term);
    output_newline out;
  ) terms;
  output_newline out


let emit_ml_tokens out dcl terms =
  emit_ml_prologue dcl;
  emit_ml_prologue out;

  (* emit token type declaration in both mli and ml *)
  emit_ml_token_type dcl terms;
  emit_ml_token_type out terms;

  (* emit declarations for token functions *)
  output_endline dcl "val name : t -> string";
  output_endline dcl "val desc : t -> string";
  output_endline dcl "val index : t -> int";
  output_endline dcl "val sval : t -> SemanticValue.t";

  (* emit the token functions *)
  emit_ml_token_fn out "name" (fun term ->
    "\"" ^ term.tbase.name ^ "\""
  ) terms;

  emit_ml_token_fn out "desc" (fun term ->
    if term.alias <> "" then
      "\"" ^ term.alias ^ "\""
    else
      "\"" ^ term.tbase.name ^ "\""
  ) terms;

  emit_ml_token_fn out "index" (fun term ->
    string_of_int term.term_index
  ) terms;

  (* this one is special, because it uses the token's data *)
  output_endline out "let sval = function";
  Array.iter (fun term ->
    if term.tbase.semtype <> "" then (
      output_string out "  | ";
      output_string out term.tbase.name;
      output_endline out " sval -> SemanticValue.repr sval";
    )
  ) terms;
  output_endline out "  | tok -> SemanticValue.null";

  ()


(************************************************
 * :: User actions
 ************************************************)


let emit_ml_descriptions out terms nonterms =
  output_endline out "(* ------------------- description functions ------------------ *)";

  (* emit a map of terminal ids to their names *)
  output_endline out "let termNamesArray : string array = [|";
  Array.iteri (fun code term ->
    Printf.fprintf out "  \"%s\"; (* %d *)\n" term.tbase.name code;
  ) terms;
  output_endline out "|]";
  output_newline out;
  output_newline out;

  (* emit a map of nonterminal ids to their names *)
  output_endline out "let nontermNamesArray : string array = [|";
  Array.iteri (fun code nonterm ->
    Printf.fprintf out "  \"%s\"; (* %d *)\n" nonterm.nbase.name code;
  ) nonterms;
  output_endline out "|]";
  output_newline out;
  output_newline out;

  ()


let emit_ml_actions out prods =
  output_endline out "  (* ------------------- actions ------------------ *)";
  output_endline out "  reductionActionArray = [|";
  (* iterate over productions, emitting action function closures *)
  Array.iter (fun prod ->
    (* put the production in comments above the defn *)
    output_string out "    (* ";
    PrintGrammar.print_production ~out prod;
    output_endline out " *)";

    output_endline out "    (fun svals ->";

    let output_binding tag index sym =
      output_string out "      let ";
      output_string out tag;
      output_string out " = (SemanticValue.obj svals.(";
      output_int out index;
      output_string out ") : ";
      output_string out (semtype sym);
      output_endline out ") in";
    in

    (* iterate over RHS elements, emitting bindings for each with a tag *)
    let first_tagged, _ =
      List.fold_left (fun (first_tagged, index) sym ->
        let first_tagged =
          match sym with
          (* only consider elements with a tag *)
          | Terminal ("", term) -> first_tagged
          | Nonterminal ("", term) -> first_tagged

          | Terminal (tag, term) ->
              output_binding tag index term.tbase;
              if first_tagged <> "" then first_tagged else tag

          | Nonterminal (tag, nonterm) ->
              output_binding tag index nonterm.nbase;
              if first_tagged <> "" then first_tagged else tag
        in

        first_tagged, index + 1
      ) ("", 0) prod.right
    in

    (* give a name to the yielded value so we can ensure it conforms to
     * the declared type *)
    output_string out "      let __result : ";
    output_string out (semtype prod.left.nbase);
    output_string out " = ";

    (* now insert the user's code, to execute in this environment of
     * properly-typed semantic values *)
    if prod.action = "" then
      emit_ml_user_code out first_tagged
    else
      emit_ml_user_code out prod.action;

    output_newline out;
    output_endline out "      in (SemanticValue.repr __result));"; (* cast to SemanticValue.t *)
    output_newline out;

  ) prods;
  output_endline out "  |];";
  output_newline out;
  output_newline out;

  ()


let emit_ml_spec_func out name semtype rettype func id =
  match func with
  | Some { params; code } ->
      let real_rettype =
        if rettype = semtype then
          "SemanticValue.t"
        else
          rettype
      in

      output_string out "    (fun";
      List.iter (fun param ->
        output_string out " (_";
        output_string out param;
        output_string out " : SemanticValue.t)";
      ) params;
      output_endline out " ->";
      List.iter (fun param ->
        output_string out "      let ";
        output_string out param;
        output_string out " : ";
        output_string out semtype;
        output_string out " = SemanticValue.obj (_";
        output_string out param;
        output_endline out ") in"
      ) params;
      output_string out "      let __result : ";
      output_string out rettype;
      output_string out " = ";
      emit_ml_user_code out code;
      output_endline out " in";
      if real_rettype <> rettype then
        output_endline out "      SemanticValue.repr __result);"
      else
        output_endline out "      __result);"

  | None ->
      output_string out "    default_";
      output_string out name;
      output_string out " ";
      output_int out id;
      output_endline out ";"


let emit_ml_dup_del_merge out terms nonterms =
  output_endline out "  (* ------------------- dup/del/merge/keep nonterminals ------------------ *)";

  let emit sf_name a_name rettype base func syms =
    output_string out "  ";
    output_string out a_name;
    output_endline out "Array = [|";
    Array.iteri (fun i sym ->
      let paramtype = semtype (base sym) in
      let rettype = if rettype <> "" then rettype else paramtype in
      emit_ml_spec_func out sf_name paramtype rettype (func sym) i;
    ) syms;
    output_endline out "  |];";
  in

  let emit_nonterm sf_name a_name rettype func =
    emit sf_name a_name rettype
      (fun nonterm -> nonterm.nbase)
      func
      nonterms
  in

  let emit_term sf_name a_name rettype func =
    emit sf_name a_name rettype
      (fun term -> term.tbase)
      func
      terms
  in

  emit_nonterm "dup"   "duplicateNontermValue"  ""     (fun nonterm -> nonterm.nbase.dup);
  emit_nonterm "del"   "deallocateNontermValue" "unit" (fun nonterm -> nonterm.nbase.del);
  emit_nonterm "merge" "mergeAlternativeParses" ""     (fun nonterm -> nonterm.merge);
  emit_nonterm "keep"  "keepNontermValue"       "bool" (fun nonterm -> nonterm.keep);

  emit_term "dup"      "duplicateTerminalValue"  ""     (fun term -> term.tbase.dup);
  emit_term "del"      "deallocateTerminalValue" "unit" (fun term -> term.tbase.del);
  emit_term "classify" "reclassifyToken"         "int"  (fun term -> term.classify);

  ()


let emit_ml_action_code out dcl terms nonterms prods final_prod grammar tables =
  let result_type = final_semtype final_prod in

  emit_ml_prologue dcl;
  emit_ml_prologue out;

  (* insert the stand-alone verbatim sections *)
  List.iter (fun code ->
    emit_ml_user_code ~braces:false dcl code
  ) grammar.verbatim;

  (* all that goes into the interface is the name of the
   * UserActions.t object *)
  output_endline dcl "module T : UserActions.S";
  Printf.fprintf dcl "val userActions : %s UserActions.t\n" result_type;

  (* Open module so record field labels are visible *)
  output_endline out "open UserActions";
  output_newline out;
  output_newline out;

  (* stand-alone verbatim sections go into .ml file *also* *)
  List.iter (fun code ->
    emit_ml_user_code ~braces:false out code
  ) grammar.verbatim;

  emit_ml_descriptions out terms nonterms;

  (* impl_verbatim sections *)
  output_endline out "(* ------------------- impl_verbatim sections ------------------ *)";
  List.iter (fun code ->
    emit_ml_user_code ~braces:false out code
  ) grammar.impl_verbatim;
  output_newline out;
  output_newline out;

  output_endline out "let userFunctions : UserActions.functions = {";
  emit_ml_actions out prods;
  emit_ml_dup_del_merge out terms nonterms;
  output_endline out "}";
  output_newline out;
  output_newline out;

  (* main action function; uses the array emitted above *)
  output_endline out "module T : UserActions.S = struct";
  output_endline out "  let reductionAction (productionId : int) (svals : SemanticValue.t array) : SemanticValue.t =";
  output_endline out "    userFunctions.reductionActionArray.(productionId) svals";

  (* dup *)
  output_endline out "  let duplicateNontermValue (nontermId : int) (sval : SemanticValue.t) : SemanticValue.t =";
  output_endline out "    userFunctions.duplicateNontermValueArray.(nontermId) sval";
  output_endline out "  let duplicateTerminalValue (termId : int) (sval : SemanticValue.t) : SemanticValue.t =";
  output_endline out "    userFunctions.duplicateTerminalValueArray.(termId) sval";
  (* del *)
  output_endline out "  let deallocateNontermValue (nontermId : int) (sval : SemanticValue.t) : unit =";
  output_endline out "    userFunctions.deallocateNontermValueArray.(nontermId) sval";
  output_endline out "  let deallocateTerminalValue (termId : int) (sval : SemanticValue.t) : unit =";
  output_endline out "    userFunctions.deallocateTerminalValueArray.(termId) sval";
  (* merge *)
  output_endline out "  let mergeAlternativeParses (nontermId : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =";
  output_endline out "    userFunctions.mergeAlternativeParsesArray.(nontermId) left right";
  (* keep *)
  output_endline out "  let keepNontermValue (nontermId : int) (sval : SemanticValue.t) : bool =";
  output_endline out "    userFunctions.keepNontermValueArray.(nontermId) sval";
  (* classify *)
  output_endline out "  let reclassifyToken (oldTokenType : int) (sval : SemanticValue.t) : int =";
  output_endline out "    userFunctions.reclassifyTokenArray.(oldTokenType) sval";
  (* emit a function to describe terminals; at some point I'd like to
   * extend my grammar format to allow the user to supply
   * token-specific description functions, but for now I will just
   * use the information easily available to synthesise one;
   * I print "sval % 100000" so I get a 5-digit number, which is
   * easy for me to compare for equality without adding much clutter
   *
   * ML: I could do something like this using Obj, but I'd rather
   * not abuse that interface unnecessarily. *)
  output_endline out "  let terminalDescription (termId : int) (sval : SemanticValue.t) : string =";
  output_endline out "    termNamesArray.(termId)";
  (* and a function to describe nonterminals also *)
  output_endline out "  let nonterminalDescription (nontermId : int) (sval : SemanticValue.t) : string =";
  output_endline out "    nontermNamesArray.(nontermId)";
  (* emit functions to get access to the static maps *)
  output_endline out "  let terminalName (termId : int) : string =";
  output_endline out "    termNamesArray.(termId)";
  output_endline out "  let nonterminalName (nontermId : int) : string =";
  output_endline out "    nontermNamesArray.(nontermId)";
  output_endline out "end";
  output_newline out;
  output_newline out;

  (* wrap all the action stuff up as a record *)
  Printf.fprintf out "let userActions : %s UserActions.t = {\n" result_type;
  output_endline out "  reductionAction = T.reductionAction;";
  output_endline out "  duplicateTerminalValue = T.duplicateTerminalValue;";
  output_endline out "  duplicateNontermValue = T.duplicateNontermValue;";
  output_endline out "  deallocateTerminalValue = T.deallocateTerminalValue;";
  output_endline out "  deallocateNontermValue = T.deallocateNontermValue;";
  output_endline out "  mergeAlternativeParses = T.mergeAlternativeParses;";
  output_endline out "  keepNontermValue = T.keepNontermValue;";
  output_endline out "  terminalDescription = T.terminalDescription;";
  output_endline out "  nonterminalDescription = T.nonterminalDescription;";
  output_endline out "  terminalName = T.terminalName;";
  output_endline out "  nonterminalName = T.nonterminalName;";
  output_endline out "}"


(************************************************
 * :: Parse tables
 ************************************************)


let emit_ml_tables out dcl dat tables =
  TablePrinting.dump_tables dat tables;

  emit_ml_prologue dcl;
  output_endline dcl "val parseTables : ParseTablesType.t";

  emit_ml_prologue out;
  if true then (
    output_endline out "let parseTables : ParseTablesType.t = Marshal.from_channel (open_in_bin \"_build/ccparse/gr/ccTables.dat\")";
  ) else (
    TablePrinting.print_tables out tables
  )


(************************************************
 * :: Main entry point
 ************************************************)

let emit_ml name env grammar tables =
  let open AnalysisEnvType in
  let terms = env.indexed_terms in
  let nonterms = env.indexed_nonterms in
  let prods = env.indexed_prods in
  let final_prod = env.indexed_prods.(tables.ParseTablesType.finalProductionIndex) in

  (* Tokens *)
  let dcl = open_out (name ^ "Tokens.mli") in
  let out = open_out (name ^ "Tokens.ml") in
  closing (emit_ml_tokens out dcl) terms
    [dcl; out];

  (* Actions *)
  let dcl = open_out (name ^ "Actions.mli") in
  let out = open_out (name ^ "Actions.ml") in
  closing (emit_ml_action_code out dcl terms nonterms prods final_prod grammar) tables
    [dcl; out];

  (* Tables *)
  let dcl = open_out     (name ^ "Tables.mli") in
  let out = open_out     (name ^ "Tables.ml") in
  let dat = open_out_bin (name ^ "Tables.dat") in

  closing (emit_ml_tables out dcl dat) tables
    [dcl; out; dat];
