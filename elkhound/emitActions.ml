open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Common helpers
 ************************************************)


let fold_bindings =
  List.fold_left (fun code binding ->
    <:expr<let $binding$ in $code$>>
  )


(************************************************
 * :: Semantic type helpers
 ************************************************)


let semtype sym =
  match sym.semtype with
  | None ->
      <:ctyp<'$lid:"t" ^ sym.name$>>
  | Some semtype ->
      semtype


let final_semtype final_prod =
  match final_prod.right with
  | Nonterminal (_, { nbase = { semtype } }) :: _ ->
      begin match semtype with
      | None ->
          failwith "final nonterminal needs defined type"
      | Some <:ctyp<'$_$>> ->
          failwith "final nonterminal cannot be polymorphic"
      | Some semtype ->
          semtype
      end

  | _ ->
      failwith "could not find final nonterminal"


(************************************************
 * :: User actions
 ************************************************)


(* ------------------- description functions ------------------ *)
let make_ml_descriptions terms nonterms =
  (* emit a map of terminal ids to their names *)
  let term_names_array =
    let names =
      Array.map (fun term -> <:expr<$str:term.tbase.name$>>) terms
      |> Array.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let termNamesArray : string array = [| $names$ |]>>
  in

  (* emit a map of nonterminal ids to their names *)
  let nonterm_names_array =
    let names =
      Array.map (fun nonterm -> <:expr<$str:nonterm.nbase.name$>>) nonterms
      |> Array.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let nontermNamesArray : string array = [| $names$ |]>>
  in

  <:str_item<
    $term_names_array$
    $nonterm_names_array$
  >>


(* ------------------- actions ------------------ *)
let make_ml_actions prods =
  (* iterate over productions, emitting action function closures *)
  let closures =
    Array.map (fun prod ->
      (* put the production in comments above the defn *)
      if false then (
        print_string "(*";
        PrintGrammar.print_production prod;
        print_endline " *)";
      );

      let make_binding tag index sym =
        assert (is_lid tag);
        <:binding<$lid:tag$ = (SemanticValue.obj svals.($int:string_of_int index$) : $semtype sym$)>>
      in

      (* iterate over RHS elements, emitting bindings for each with a tag *)
      let bindings =
        BatList.mapi (fun index sym ->
          match sym with
          | Terminal ("", _)
          | Nonterminal ("", _) ->
              (* only consider elements with a tag *)
              []

          | Terminal (tag, term) ->
              [make_binding tag index term.tbase]

          | Nonterminal (tag, nonterm) ->
              [make_binding tag index nonterm.nbase]
        ) prod.right
        |> List.concat
      in

      let action_code =
        match prod.action with
        | None ->
            begin match bindings with
            | [ <:binding<$lid:first_tagged$ = $_$>> ] ->
                <:expr<$lid:first_tagged$>>

            | [binding] ->
                failwith "invalid name binding format"

            | binding :: _ ->
                (*PrintGrammar.print_production prod;*)
                (* TODO: move this to a semantic check phase *)
                failwith "production with more than one binding must provide action code"

            | [] ->
                (*PrintGrammar.print_production prod;*)
                (* TODO: this, too *)
                failwith "no name bindings in production with default action"
            end
        | Some code ->
            code
      in

      (* give a name to the yielded value so we can ensure it conforms to
       * the declared type *)
      let result =
        <:expr<
          (* now insert the user's code, to execute in this environment of
           * properly-typed semantic values *)
          let __result : $semtype prod.left.nbase$ = $action_code$ in
          (* cast to SemanticValue.t *)
          SemanticValue.repr __result
        >>
      in

      let fun_body = fold_bindings result bindings in
      
      <:expr<fun svals start_p end_p -> $fun_body$>>
    ) prods

    |> Array.to_list
    |> Ast.exSem_of_list
  in

  <:rec_binding<reductionActionArray = [| $closures$ |]>>


let make_ml_spec_func name semtype rettype kind func id =
  match func with
  | None ->
      <:expr<$lid:"default_" ^ name$ $lid:kind ^ "NamesArray"$ ($int:string_of_int id$)>>

  | Some { params; code } ->
      let real_rettype =
        if rettype = semtype then
          <:ctyp<SemanticValue.t>>
        else
          rettype
      in

      let untyped_params =
        List.rev_map (fun param ->
          <:patt<($lid:"_" ^ param$ : SemanticValue.t)>>
        ) params
      in

      let bindings =
        <:binding<__result : $rettype$ = $code$>>
        :: List.rev_map (fun param ->
          <:binding<($lid:param$ : $semtype$) = SemanticValue.obj $lid:"_" ^ param$>>
        ) params
      in

      let result =
        if real_rettype != rettype then
          <:expr<SemanticValue.repr __result>>
        else
          <:expr<__result>>
      in

      let fun_body = fold_bindings result bindings in

      List.fold_left (fun code param ->
        <:expr<fun $param$ -> $code$>>
      ) fun_body untyped_params


let make_ml_dup_del_merge terms nonterms =

  let make sf_name a_name rettype kind base func syms =
    let closures =
      Array.mapi (fun i sym ->
        let paramtype = semtype (base sym) in
        let rettype = BatOption.default paramtype rettype in
        make_ml_spec_func sf_name paramtype rettype kind (func sym) i
      ) syms

      |> Array.to_list
      |> Ast.exSem_of_list
    in

    assert (is_lid a_name);
    <:rec_binding<$lid:a_name ^ "Array"$ = [| $closures$ |]>>
  in

  let make_nonterm sf_name a_name func rettype =
    make sf_name a_name rettype
      "nonterm"
      (fun nonterm -> nonterm.nbase)
      func
      nonterms
  in

  let make_term sf_name a_name func rettype =
    make sf_name a_name rettype
      "term"
      (fun term -> term.tbase)
      func
      terms
  in

  [
    (* ------------------- dup/del/merge/keep nonterminals ------------------ *)
    make_nonterm "dup"   "duplicateNontermValue"  (fun nonterm -> nonterm.nbase.dup) None;
    make_nonterm "del"   "deallocateNontermValue" (fun nonterm -> nonterm.nbase.del) (Some <:ctyp<unit>>);
    make_nonterm "merge" "mergeAlternativeParses" (fun nonterm -> nonterm.merge) 	   None;
    make_nonterm "keep"  "keepNontermValue"       (fun nonterm -> nonterm.keep) 	   (Some <:ctyp<bool>>);

    (* ------------------- dup/del/classify terminals ------------------ *)
    make_term "dup"      "duplicateTerminalValue"  (fun term -> term.tbase.dup) None;
    make_term "del"      "deallocateTerminalValue" (fun term -> term.tbase.del) (Some <:ctyp<unit>>);
    make_term "classify" "reclassifyToken"         (fun term -> term.classify)  (Some <:ctyp<int>>);
  ]


let make_ml_action_code terms nonterms prods_by_lhs final_prod verbatims impl_verbatims =
  let prod_count = Array.fold_left (List.fold_left (fun count _ -> count + 1)) 0 prods_by_lhs in
  let prods = Array.make prod_count empty_production in
  Array.iter (List.iter (fun prod -> prods.(prod.prod_index) <- prod)) prods_by_lhs;

  let result_type = final_semtype (final_prod prods) in

  let closures =
    make_ml_actions prods
    :: make_ml_dup_del_merge terms nonterms
    |> Ast.rbSem_of_list
  in

  <:sig_item<
    (* insert the stand-alone verbatim sections *)
    $Ast.sgSem_of_list verbatims$

    (* all that goes into the interface is the name of the
     * UserActions.t object *)
    val userActions : $result_type$ UserActions.t
  >>,
  <:str_item<
    (* Open module so record field labels are visible *)
    open UserActions

    (* impl_verbatim sections *)
    $Ast.stSem_of_list impl_verbatims$

    $make_ml_descriptions terms nonterms$

    let userFunctions : UserActions.functions = { $closures$ }

    (* main action function; uses the array emitted above *)
    let reductionAction (productionId : int) (svals : SemanticValue.t array) (start_p : Lexing.position) (end_p : Lexing.position) : SemanticValue.t =
      userFunctions.reductionActionArray.(productionId) svals start_p end_p

    (* dup *)
    let duplicateNontermValue (nontermId : int) (sval : SemanticValue.t) : SemanticValue.t =
      userFunctions.duplicateNontermValueArray.(nontermId) sval
    let duplicateTerminalValue (termId : int) (sval : SemanticValue.t) : SemanticValue.t =
      userFunctions.duplicateTerminalValueArray.(termId) sval
    (* del *)
    let deallocateNontermValue (nontermId : int) (sval : SemanticValue.t) : unit =
      userFunctions.deallocateNontermValueArray.(nontermId) sval
    let deallocateTerminalValue (termId : int) (sval : SemanticValue.t) : unit =
      userFunctions.deallocateTerminalValueArray.(termId) sval
    (* merge *)
    let mergeAlternativeParses (nontermId : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =
      userFunctions.mergeAlternativeParsesArray.(nontermId) left right
    (* keep *)
    let keepNontermValue (nontermId : int) (sval : SemanticValue.t) : bool =
      userFunctions.keepNontermValueArray.(nontermId) sval
    (* classify *)
    let reclassifyToken (oldTokenType : int) (sval : SemanticValue.t) : int =
      userFunctions.reclassifyTokenArray.(oldTokenType) sval
    (* emit a function to describe terminals; at some point I'd like to
     * extend my grammar format to allow the user to supply
     * token-specific description functions, but for now I will just
     * use the information easily available to synthesise one;
     * I print "sval % 100000" so I get a 5-digit number, which is
     * easy for me to compare for equality without adding much clutter
     *
     * ML: I could do something like this using Obj, but I'd rather
     * not abuse that interface unnecessarily. *)
    let terminalDescription (termId : int) (sval : SemanticValue.t) : string =
      termNamesArray.(termId)
    (* and a function to describe nonterminals also *)
    let nonterminalDescription (nontermId : int) (sval : SemanticValue.t) : string =
      nontermNamesArray.(nontermId)
    (* emit functions to get access to the static maps *)
    let terminalName (termId : int) : string =
      termNamesArray.(termId)
    let nonterminalName (nontermId : int) : string =
      nontermNamesArray.(nontermId)

    (* wrap all the action stuff up as a record *)
    let userActions : $result_type$ UserActions.t = {
      reductionAction;
      duplicateTerminalValue;
      duplicateNontermValue;
      deallocateTerminalValue;
      deallocateNontermValue;
      mergeAlternativeParses;
      keepNontermValue;
      reclassifyToken;
      terminalDescription;
      nonterminalDescription;
      terminalName;
      nonterminalName;
    }
  >>
