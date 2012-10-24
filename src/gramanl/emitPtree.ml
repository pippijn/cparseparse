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


let ctyp_of_right_symbol head_tail =
  PtreeMaker.symbols_of_production head_tail
  |> List.rev
  |> List.hd
  |> ctyp_of_symbol


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
  (* nonterminal with a single production that is a tagged symbol
   * (and there is no merge) *)
  | [prod] when PtreeMaker.is_singleton_nonterminal prod && not has_merge ->
      let semtype = ctyp_of_right_symbol prod in
      <:sig_item<type t = ($semtype$)>>,
      <:str_item<type t = ($semtype$)>>

  | [tail; head_tail] when PtreeMaker.is_list_nonterminal tail head_tail && not has_merge ->
      let semtype = ctyp_of_right_symbol head_tail in
      <:sig_item<type t = ($semtype$ list)>>,
      <:str_item<type t = ($semtype$ list)>>

  | [none; some] when PtreeMaker.is_option_nonterminal none some && not has_merge ->
      let semtype = ctyp_of_right_symbol some in
      <:sig_item<type t = ($semtype$ option)>>,
      <:str_item<type t = ($semtype$ option)>>

  | [none; some] when PtreeMaker.is_boolean_nonterminal none some && not has_merge ->
      <:sig_item<type t = bool>>,
      <:str_item<type t = bool>>

  | prods ->
      let types =
        List.map (fun prod ->
          if false then (
            print_string "    (*";
            PrintGrammar.print_production prod;
            print_endline " *)";
          );

          let prod_type =
            [ <:ctyp<Glr.SourceLocation.t>> ]
            :: List.map (fun sym ->
              match sym with
              | Nonterminal ("", _)
              | Terminal ("", _) ->
                  (* nothing to do for untagged symbols *)
                  []

              | sym ->
                  [ctyp_of_symbol sym]

            ) prod.right
            |> List.concat
            |> Ast.tyAnd_of_list
          in

          let prod_name =
            match prod.prod_name with
            | None      -> "P" ^ StateId.Production.to_string prod.prod_index
            | Some name -> assert (is_uid name); name
          in

          let prod_variant =
            <:ctyp<$uid:prod_name$ of $prod_type$>>
          in

          <:ctyp<$prod_variant$>>
        ) prods
      in

      let types = Ast.tyOr_of_list (merge_types @ types) in

      (* TODO: with sexp *)
      <:sig_item<type t = $types$ | SEXP>>,
      <:str_item<type t = $types$ | SEXP>>


let make_ml_parse_tree prods prods_by_lhs reachable =
  let types =
    NtArray.fold_right (fun indices types ->
      match List.map (ProdArray.get prods) indices with
      | [] ->
          (* the empty nonterminal has no productions *)
          types

      | { left = { nbase = { name } } } :: _ when name.[0] = '_' ->
          (* we do not emit code for the synthesised start rule *)
          types

      | first :: _ as prods ->
          let nonterm = first.left in
          let name = nonterm.nbase.name in
          assert (is_uid name);

          if not (StringSet.mem name reachable) then (
            if Options._trace_unreachable_ptree () then
              print_endline ("unreachable: " ^ name);
            types
          ) else (
            let has_merge = nonterm.merge != None in

            let intf_types, impl_types = production_types has_merge prods in

            (name, intf_types, impl_types) :: types
          )

    ) prods_by_lhs []
  in

  let bindings =
    List.fold_left (fun bindings (name, intf_types, impl_types) ->
      (* signature *)
      let intf =
        <:module_type<
          sig
            $intf_types$
            include Sig.ConvertibleType with type t := t
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

      assert (not (StringMap.mem name bindings));
      StringMap.add name <:module_binding<$name$ : $intf$ = $impl$>> bindings
    ) StringMap.empty types
  in

  let first_module =
    match types with
    | (name, _, _) :: _ ->
        name
    | _ ->
        failwith "could not find first module"
  in

  let combined =
    BatList.reduce (fun combined binding ->
      <:module_binding<$combined$ and $binding$>>
    ) (StringMap.bindings bindings |> List.split |> snd)
  in

  let modules = Ast.StRecMod (_loc, combined) in

  let impl =
    <:str_item<
      open Sexplib.Conv

      module Ptree = struct
        $modules$

        type t = $uid:first_module$.t
        let t_of_sexp = $uid:first_module$.t_of_sexp
        let sexp_of_t = $uid:first_module$.sexp_of_t
      end
    >>
  in

  impl
