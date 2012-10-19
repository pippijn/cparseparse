open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


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

  (* emit a map of terminal ids to their aliases *)
  let term_aliases_array =
    let names =
      Array.map (fun term -> <:expr<$str:GrammarUtil.name_of_terminal term$>>) terms
      |> Array.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let termAliasesArray : string array = [| $names$ |]>>
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

  <:sig_item<
    val termNamesArray : string array
    val termAliasesArray : string array
    val nontermNamesArray : string array
  >>,
  <:str_item<
    $term_names_array$
    $term_aliases_array$
    $nonterm_names_array$
  >>