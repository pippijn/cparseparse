open Camlp4
open Camlp4.PreCast
open GrammarType

module OCamlPrinter = Camlp4.PreCast.Printers.OCaml


(************************************************
 * :: Output helpers
 ************************************************)

let _loc = Loc.ghost

let output_newline out =
  output_char out '\n'

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
  let node = <:ctyp<'$lid:"t" ^ sym.name$>> in
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


let emit_ml_token_type terms =
  Array.fold_left (fun types term ->
    let semtype =
      <:ctyp<$uid:term.tbase.name$>>
    in

    let semtype =
      match term.tbase.semtype with
      | "" -> semtype
      | sv -> <:ctyp<$semtype$ of $lid:sv$>>
    in

    <:ctyp<$types$ | $semtype$>>
  ) <:ctyp<>> terms


let emit_ml_token_fn ?default value terms =
  let cases =
    Array.fold_left (fun cases term ->
      try
        let name = term.tbase.name in

        let patt =
          match term.tbase.semtype with
          | "" -> <:patt<$uid:name$>>
          | _  -> <:patt<$uid:name$ sval>>
        in

        let case =
          <:match_case<$patt$ -> $value term$>>
        in

        <:match_case<$cases$ | $case$>>
      with Exit ->
        cases
    ) <:match_case<>> terms
  in

  let cases =
    match default with
    | None -> cases
    | Some case -> <:match_case<$cases$ | $case$>>
  in

  <:expr<function $cases$>>


let emit_ml_tokens terms =
  (* emit token type declaration in both mli and ml *)
  let types = emit_ml_token_type terms in
  let intf =
    <:sig_item<
      type t = $types$
      include Lexerint.TokenInfo with type t := t
    >>
  in

  (* emit the token functions *)
  let name_fn =
    emit_ml_token_fn (fun term ->
      <:expr<$str:term.tbase.name$>>
    ) terms
  in

  let desc_fn =
    emit_ml_token_fn (fun { alias; tbase = { name } } ->
      match alias with
      | ""    -> <:expr<$str:name$>>
      | alias -> <:expr<$str:alias$>>
    ) terms
  in

  let index_fn =
    emit_ml_token_fn (fun term ->
      <:expr<$int:string_of_int term.term_index$>>
    ) terms
  in

  let sval_fn =
    emit_ml_token_fn (fun term ->
      match term.tbase.semtype with
      | "" -> raise Exit
      | _  -> <:expr<SemanticValue.repr sval>>
    ) terms ~default:<:match_case<tok -> SemanticValue.null>>
  in

  let impl =
    <:str_item<
      type t = $types$
      let name = $name_fn$
      let desc = $desc_fn$
      let index = $index_fn$
      let sval = $sval_fn$
    >>
  in

  intf, impl


(************************************************
 * :: Concrete syntax tree
 ************************************************)

let and_type typ1 typ2 =
  match typ1 with
  | <:ctyp<>> ->
      typ2
  | typ1 ->
      Ast.TyAnd (_loc, typ1, typ2)


let sym_type = function
  | Nonterminal (tag, nonterm) ->
      (* the type is the referenced nonterminal module *)
      let typ = nonterm.nbase.name in
      assert (typ <> "");
      <:ctyp<$uid:typ$.t>>

  | Terminal (tag, term) ->
      (* use the terminal type. tagged terminals must have a declared type *)
      let typ = term.tbase.semtype in
      assert (typ <> "");
      <:ctyp<$lid:typ$>>


let production_types has_merge prods =
  let types =
    if has_merge then
      <:ctyp<Merge of t * t>>
    else
      <:ctyp<>>
  in

  match prods with
  (* nonterminal with a single production that is a tagged terminal *)
  | [{ right = [Terminal (tag, { tbase = { semtype } })] }] when tag <> "" ->
      assert (not has_merge);
      assert (semtype <> "");
      <:sig_item<type t = $lid:semtype$>>,
      <:str_item<type t = $lid:semtype$>>

  | prods ->
      let types =
        List.fold_left (fun types prod ->
          if false then (
            print_string "    (*";
            PrintGrammar.print_production prod;
            print_endline " *)";
          );

          let prod_type =
            List.fold_left (fun prod_type sym ->
              match sym with
              | Nonterminal ("", _)
              | Terminal ("", _) ->
                  (* nothing to do for untagged symbols *)
                  prod_type

              | sym ->
                  and_type prod_type (sym_type sym)

            ) <:ctyp<>> prod.right
          in

          let prod_name =
            match prod.prod_name with
            | None      -> "P" ^ string_of_int prod.prod_index
            | Some name -> assert (name <> ""); name
          in

          let prod_variant =
            match prod_type with
            | <:ctyp<>> -> <:ctyp<$uid:prod_name$>>
            | _	      -> <:ctyp<$uid:prod_name$ of $prod_type$>>
          in

          <:ctyp<$prod_variant$ | $types$>>
        ) types prods
      in

      (* TODO: with sexp *)
      <:sig_item<type t = $types$>>,
      <:str_item<type t = $types$ | SEXP>>



let emit_ml_parse_tree prods_by_lhs =
  let bindings =
    List.rev (Array.fold_left (fun bindings prods ->
      match prods with
      | [] ->
          (* the empty nonterminal has no productions *)
          bindings

      | { left = { nbase = { name } } } :: _ when name.[0] = '_' ->
          (* we do not emit code for the synthesised start rule *)
          bindings

      | first :: _ ->
          let nonterm = first.left in
          let name = nonterm.nbase.name in

          let has_merge = nonterm.merge != None in

          let intf_types, impl_types = production_types has_merge prods in

          (* signature *)
          let intf =
            <:module_type<
              sig
                $intf_types$
                val sexp_of_t : t -> Sexplib.Sexp.t
                val t_of_sexp : Sexplib.Sexp.t -> t
              end
            >>
          in

          (* implementation *)
          let impl =
            <:module_expr<
              struct
                $impl_types$
              end
            >>
          in

          let binding =
            <:module_binding<$name$ : $intf$ = $impl$>>
          in

          binding :: bindings
    ) [] prods_by_lhs)
  in

  let combined =
    BatList.reduce (fun combined binding ->
      Ast.MbAnd (_loc, binding, combined)
    ) (List.rev bindings)
  in

  let first_module =
    match bindings with
    | <:module_binding<$name$ : $_$ = $_$>> :: _ ->
        name
    | _ ->
        failwith "could not find first module"
  in

  <:str_item<
    open Sexplib.Conv

    module rec $combined$

    type t = $uid:first_module$.t
    let t_of_sexp = $uid:first_module$.t_of_sexp
    let sexp_of_t = $uid:first_module$.sexp_of_t
  >>


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
    output_string out "    (*";
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


let emit_ml_action_code out dcl terms nonterms prods final_prod verbatims impl_verbatims =
  let result_type = final_semtype (final_prod prods) in

  emit_ml_prologue dcl;
  emit_ml_prologue out;

  (* insert the stand-alone verbatim sections *)
  List.iter (fun code ->
    emit_ml_user_code ~braces:false dcl code
  ) verbatims;

  (* all that goes into the interface is the name of the
   * UserActions.t object *)
  Printf.fprintf dcl "val userActions : %s UserActions.t\n" result_type;

  (* Open module so record field labels are visible *)
  output_endline out "open UserActions";
  output_newline out;
  output_newline out;

  (* stand-alone verbatim sections go into .ml file *also* *)
  List.iter (fun code ->
    emit_ml_user_code ~braces:false out code
  ) verbatims;

  emit_ml_descriptions out terms nonterms;

  (* impl_verbatim sections *)
  output_endline out "(* ------------------- impl_verbatim sections ------------------ *)";
  List.iter (fun code ->
    emit_ml_user_code ~braces:false out code
  ) impl_verbatims;
  output_newline out;
  output_newline out;

  output_endline out "let userFunctions : UserActions.functions = {";
  emit_ml_actions out prods;
  emit_ml_dup_del_merge out terms nonterms;
  output_endline out "}";
  output_newline out;
  output_newline out;

  (* main action function; uses the array emitted above *)
  output_endline out "let reductionAction (productionId : int) (svals : SemanticValue.t array) : SemanticValue.t =";
  output_endline out "  userFunctions.reductionActionArray.(productionId) svals";

  (* dup *)
  output_endline out "let duplicateNontermValue (nontermId : int) (sval : SemanticValue.t) : SemanticValue.t =";
  output_endline out "  userFunctions.duplicateNontermValueArray.(nontermId) sval";
  output_endline out "let duplicateTerminalValue (termId : int) (sval : SemanticValue.t) : SemanticValue.t =";
  output_endline out "  userFunctions.duplicateTerminalValueArray.(termId) sval";
  (* del *)
  output_endline out "let deallocateNontermValue (nontermId : int) (sval : SemanticValue.t) : unit =";
  output_endline out "  userFunctions.deallocateNontermValueArray.(nontermId) sval";
  output_endline out "let deallocateTerminalValue (termId : int) (sval : SemanticValue.t) : unit =";
  output_endline out "  userFunctions.deallocateTerminalValueArray.(termId) sval";
  (* merge *)
  output_endline out "let mergeAlternativeParses (nontermId : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =";
  output_endline out "  userFunctions.mergeAlternativeParsesArray.(nontermId) left right";
  (* keep *)
  output_endline out "let keepNontermValue (nontermId : int) (sval : SemanticValue.t) : bool =";
  output_endline out "  userFunctions.keepNontermValueArray.(nontermId) sval";
  (* classify *)
  output_endline out "let reclassifyToken (oldTokenType : int) (sval : SemanticValue.t) : int =";
  output_endline out "  userFunctions.reclassifyTokenArray.(oldTokenType) sval";
  (* emit a function to describe terminals; at some point I'd like to
   * extend my grammar format to allow the user to supply
   * token-specific description functions, but for now I will just
   * use the information easily available to synthesise one;
   * I print "sval % 100000" so I get a 5-digit number, which is
   * easy for me to compare for equality without adding much clutter
   *
   * ML: I could do something like this using Obj, but I'd rather
   * not abuse that interface unnecessarily. *)
  output_endline out "let terminalDescription (termId : int) (sval : SemanticValue.t) : string =";
  output_endline out "  termNamesArray.(termId)";
  (* and a function to describe nonterminals also *)
  output_endline out "let nonterminalDescription (nontermId : int) (sval : SemanticValue.t) : string =";
  output_endline out "  nontermNamesArray.(nontermId)";
  (* emit functions to get access to the static maps *)
  output_endline out "let terminalName (termId : int) : string =";
  output_endline out "  termNamesArray.(termId)";
  output_endline out "let nonterminalName (nontermId : int) : string =";
  output_endline out "  nontermNamesArray.(nontermId)";
  output_newline out;
  output_newline out;

  (* wrap all the action stuff up as a record *)
  Printf.fprintf out "let userActions : %s UserActions.t = {\n" result_type;
  output_endline out "  reductionAction;";
  output_endline out "  duplicateTerminalValue;";
  output_endline out "  duplicateNontermValue;";
  output_endline out "  deallocateTerminalValue;";
  output_endline out "  deallocateNontermValue;";
  output_endline out "  mergeAlternativeParses;";
  output_endline out "  keepNontermValue;";
  output_endline out "  reclassifyToken;";
  output_endline out "  terminalDescription;";
  output_endline out "  nonterminalDescription;";
  output_endline out "  terminalName;";
  output_endline out "  nonterminalName;";
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

let emit_ml name terms nonterms prods prods_by_lhs verbatims impl_verbatims tables =
  let final_prod prods = prods.(tables.ParseTablesType.finalProductionIndex) in

  (* Tokens *)
  let dcl = name ^ "Tokens.mli" in
  let out = name ^ "Tokens.ml" in
  let intf, impl = emit_ml_tokens terms in

  OCamlPrinter.print_interf ~output_file:dcl intf;
  OCamlPrinter.print_implem ~output_file:out impl;


  (* Parse Tree *)
  let out = name ^ "Ptree.ml" in
  let impl = emit_ml_parse_tree prods_by_lhs in

  OCamlPrinter.print_implem ~output_file:out impl;
  (* TODO: with sexp *)
  Sys.command ("sed -i -e 's/type t = string;;/type t = string with sexp;;/' " ^ out);
  Sys.command ("sed -i -e 's/ | SEXP;;/ with sexp;;/' " ^ out);


  (* Parse Tree Actions *)
  let dcl = open_out (name ^ "PtreeActions.mli") in
  let out = open_out (name ^ "PtreeActions.ml") in
  closing (emit_ml_action_code out dcl
      terms
      (PtreeMaker.nonterms nonterms)
      (PtreeMaker.prods prods)
      final_prod verbatims) impl_verbatims
    [dcl; out];

  (* Actions *)
  let dcl = open_out (name ^ "Actions.mli") in
  let out = open_out (name ^ "Actions.ml") in
  closing (emit_ml_action_code out dcl
      terms
      nonterms
      prods
      final_prod verbatims) impl_verbatims
    [dcl; out];

  (* Tables *)
  let dcl = open_out     (name ^ "Tables.mli") in
  let out = open_out     (name ^ "Tables.ml") in
  let dat = open_out_bin (name ^ "Tables.dat") in

  closing (emit_ml_tables out dcl dat) tables
    [dcl; out; dat];
