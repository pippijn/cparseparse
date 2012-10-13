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
  <:ctyp<$uid:typ$.t>>


let ctyp_of_terminal term =
  (* use the terminal type *)
  match term.tbase.semtype with
  | None ->
      failwith "tagged terminals must have a declared type"
  | Some ty ->
      ty


let ctyp_of_symbol = function
  | Nonterminal (_, nonterm) -> ctyp_of_nonterminal nonterm
  | Terminal    (_,    term) -> ctyp_of_terminal term


(* XXX: if this function changes its output, PtreeMaker.prods probably
 * also needs to change *)
let production_types has_merge prods =
  let merge_types =
    if has_merge then
      [ <:ctyp<Merge of t * t>> ]
    else
      []
  in

  match prods with
  (* nonterminal with a single production that is a tagged terminal *)
  | [{ right = [Terminal (tag, term)] }] when tag <> "" ->
      assert (not has_merge);
      let semtype = ctyp_of_terminal term in
      let ty_dcl = Ast.TyDcl (_loc, "t", [], semtype, []) in

      Ast.SgTyp (_loc, ty_dcl),
      Ast.StTyp (_loc, ty_dcl)

  | prods ->
      let types =
        List.map (fun prod ->
          if false then (
            print_string "    (*";
            PrintGrammar.print_production prod;
            print_endline " *)";
          );

          let prod_type =
            List.map (fun sym ->
              match sym with
              | Nonterminal ("", _)
              | Terminal ("", _) ->
                  (* nothing to do for untagged symbols *)
                  []

              | sym ->
                  [ctyp_of_symbol sym]

            ) prod.right |> List.concat |> Ast.tyAnd_of_list
          in

          let prod_name =
            match prod.prod_name with
            | None      -> "P" ^ string_of_int prod.prod_index
            | Some name -> assert (is_uid name); name
          in

          let prod_variant =
            match prod_type with
            | <:ctyp<>> -> <:ctyp<$uid:prod_name$>>
            | _	      -> <:ctyp<$uid:prod_name$ of $prod_type$>>
          in

          <:ctyp<$prod_variant$>>
        ) prods
      in

      let types = Ast.tyOr_of_list (merge_types @ types) in

      (* TODO: with sexp *)
      <:sig_item<type t = $types$>>,
      <:str_item<type t = $types$ | SEXP>>



let make_ml_parse_tree prods_by_lhs =
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
          assert (is_uid name);

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

  let modules = Ast.StRecMod (_loc, combined) in

  let impl =
    <:str_item<
      open Sexplib.Conv

      $modules$

      type t = $uid:first_module$.t
      let t_of_sexp = $uid:first_module$.t_of_sexp
      let sexp_of_t = $uid:first_module$.sexp_of_t
    >>
  in

  impl


