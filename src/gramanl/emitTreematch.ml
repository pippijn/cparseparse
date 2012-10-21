open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Concrete syntax tree
 ************************************************)

let ctyp_of_nonterminal nonterm =
  (* the type is the referenced nonterminal module *)
  let typ = nonterm.nbase.name in
  assert (is_uid typ);
  typ


let ctyp_of_terminal term =
  (* use the terminal type *)
  match term.tbase.semtype with
  | None ->
      failwith "tagged terminals must have a declared type"
  | Some ty ->
      term.tbase.name


let ctyp_of_symbol = function
  | Nonterminal (_, nonterm) -> ctyp_of_nonterminal nonterm
  | Terminal    (_,    term) -> ctyp_of_terminal term


let ctyp_of_right_symbol head_tail =
  PtreeMaker.symbols_of_production head_tail
  |> List.rev
  |> List.hd
  |> ctyp_of_symbol


(* XXX: if this function changes its output, PtreeMaker.prods probably
 * also needs to change *)
let production_types term_mods left has_merge prods =
  match prods with
  | [prod] when PtreeMaker.is_singleton_nonterminal prod && not has_merge ->
      let semtype = ctyp_of_right_symbol prod in
      left ^ ": =" ^ semtype

  | [tail; head_tail] when PtreeMaker.is_list_nonterminal tail head_tail && not has_merge ->
      let semtype = ctyp_of_right_symbol head_tail in
      left ^ ": [" ^ semtype ^ "]"

  | [none; some] when PtreeMaker.is_option_nonterminal none some && not has_merge ->
      let semtype = ctyp_of_right_symbol some in
      left ^ ": ?" ^ semtype

  | [none; some] when PtreeMaker.is_boolean_nonterminal none some && not has_merge ->
      left ^ ": bool"

  | prods ->
      let types =
        List.map (fun prod ->
          let prod_type =
            List.map (fun sym ->
              match sym with
              | Nonterminal ("", _)
              | Terminal ("", _) ->
                  (* nothing to do for untagged symbols *)
                  []

              | Nonterminal (_, nonterm) ->
                  [ctyp_of_nonterminal nonterm]
              | Terminal (_, ({ tbase = { name; semtype = Some semtype; } } as term)) ->
                  if not (Hashtbl.mem term_mods name) then
                    Hashtbl.add term_mods name (CamlAst.string_of_ctyp semtype);
                  [ctyp_of_terminal term]

              | _ -> failwith "bad"

            ) prod.right
            |> List.concat
            |> String.concat " "
          in

          let prod_name =
            match prod.prod_name with
            | None      -> "P" ^ StateId.Production.to_string prod.prod_index
            | Some name -> assert (is_uid name); name
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



let make_ml_treematch reachable prods prods_by_lhs =
  let term_mods = Hashtbl.create 13 in

  let bindings =
    List.rev (NtArray.fold_left (fun bindings indices ->
      match List.map (ProdArray.get prods) indices with
      | [] ->
          (* the empty nonterminal has no productions *)
          bindings

      | { left = { nbase = { name } } } :: _ when name.[0] = '_' ->
          (* we do not emit code for the synthesised start rule *)
          bindings

      | first :: _ as prods ->
          let nonterm = first.left in
          let name = nonterm.nbase.name in
          assert (is_uid name);

          if not (StringSet.mem name reachable) then
            bindings
          else
            let has_merge = nonterm.merge != None in
            let types = production_types term_mods name has_merge prods in
            types :: bindings

    ) [] prods_by_lhs)
  in

  let term_bindings =
    Hashtbl.fold (fun name typ bindings ->
      (name ^ ": " ^ typ) :: bindings
    ) term_mods []
  in

  "ast Ptree {\n"
  ^ String.concat "\n" bindings
  ^ "\n\n"
  ^ String.concat "\n" term_bindings
  ^ "\n}\n"
