open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Concrete syntax tree
 ************************************************)

let ctyp_of_nonterminal nonterms nonterm =
  let nonterm = NtArray.get nonterms nonterm in
  (* the type is the referenced nonterminal module *)
  let typ = nonterm.nbase.name in
  assert (CodegenHelpers.is_uid typ);
  typ


let ctyp_of_terminal terms term =
  let term = TermArray.get terms term in
  (* use the terminal type *)
  match Semantic.semtype_of_term SemanticVariant.User term with
  | None ->
      failwith "tagged terminals must have a declared type"
  | Some ty ->
      term.tbase.name


let ctyp_of_symbol index = function
  | Nonterminal (_, nonterm) -> ctyp_of_nonterminal index.nonterms nonterm
  | Terminal    (_,    term) -> ctyp_of_terminal index.terms term


let ctyp_of_right_symbol index head_tail =
  PtreeMaker.right_symbol head_tail
  |> ctyp_of_symbol index


(* XXX: if this function changes its output, PtreeMaker.prods probably
 * also needs to change *)
let production_types index term_mods left has_merge prods =
  let add_term_mod = function
    | Terminal (None, term_index) ->
        assert false

    | Terminal (Some tag, term_index) ->
        let term = TermArray.get index.terms term_index in
        begin match Semantic.semtype_of_term SemanticVariant.User term with
        | Some semtype ->
            if not (Hashtbl.mem term_mods term.tbase.name) then
              Hashtbl.add term_mods term.tbase.name (CamlAst.string_of_ctyp semtype)
        | _ -> ()
        end
    | _ -> ()
  in

  match prods with
  | [prod] when PtreeMaker.is_singleton_nonterminal prod && not has_merge ->
      let right = PtreeMaker.right_symbol prod in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      left ^ ": =" ^ semtype

  | [tail; head_tail] when PtreeMaker.is_list_nonterminal tail head_tail && not has_merge ->
      let right = PtreeMaker.right_symbol head_tail in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      left ^ ": =[" ^ semtype ^ "]"

  | [none; some] when PtreeMaker.is_option_nonterminal none some && not has_merge ->
      let right = PtreeMaker.right_symbol some in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      left ^ ": =?" ^ semtype

  | [none; some] when PtreeMaker.is_boolean_nonterminal none some && not has_merge ->
      left ^ ": bool"

  | prods ->
      let types =
        List.map (fun prod ->
          let prod_type =
            List.map (fun sym ->
              match sym with
              | Nonterminal (None, _)
              | Terminal (None, _) ->
                  (* nothing to do for untagged symbols *)
                  []

              | _ ->
                  add_term_mod sym;
                  [ctyp_of_symbol index sym]

            ) prod.right
            |> List.concat
            |> String.concat " "
          in

          let prod_name =
            match prod.pbase.name with
            | ""   -> "P" ^ Ids.Production.to_string prod.pbase.index_id
            | name -> assert (is_uid name); name
          in

          prod_name ^ " SourceLocation " ^ prod_type
        ) prods
      in

      let types =
        if has_merge then
          ("Merge " ^ left ^ " " ^ left) :: types
        else
          types
      in

      let types = String.concat "\n\t| " types in

      left ^ ": " ^ types



let make_ml_treematch reachable index prods_by_lhs =
  let term_mods = Hashtbl.create 13 in

  let bindings =
    List.rev (NtArray.fold_left (fun bindings indices ->
      match List.map (ProdArray.get index.prods) indices with
      | [] ->
          (* the empty nonterminal has no productions *)
          bindings

      | first :: _ as prods ->
          let nonterm = NtArray.get index.nonterms first.left in
          let name = nonterm.nbase.name in

          if name.[0] = '_' then
            (* we do not emit code for the synthesised start rule *)
            bindings
          else (
            assert (is_uid name);

            if not (NtSet.mem reachable first.left) then
              bindings
            else
              let has_merge = Semantic.merge_of_nonterm SemanticVariant.User nonterm != None in
              let types = production_types index term_mods name has_merge prods in
              types :: bindings
          )

    ) [] prods_by_lhs)
  in

  let term_bindings =
    Hashtbl.fold (fun name typ bindings ->
      (name ^ ": " ^ typ) :: bindings
    ) term_mods []
  in

  "ast Ptree {\n"
  ^ String.concat "\n" bindings ^ "\n"
  ^ "\n"
  ^ String.concat "\n" term_bindings ^ "\n"
  ^ "}\n"
  ^ "map identity_default_map : Ptree => Ptree { }\n"
