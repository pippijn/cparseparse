open GrammarType


let rec action_of_prod : prod_semantic list -> CamlAst.expr option = function
  | [] ->
      None
  | [`SEM_ACTION expr] ->
      Some expr
  | _ ->
      failwith "more than one semantic action specified"

let action_of_prod prod =
  action_of_prod prod.pbase.semantic


let rec merge_of_nonterm : nonterm_semantic list -> spec_func option = function
  | [] ->
      None
  | `SEM_MERGE func :: _ ->
      Some func
  | _ :: tl ->
      merge_of_nonterm tl

let merge_of_nonterm nonterm =
  merge_of_nonterm nonterm.nbase.semantic


let rec keep_of_nonterm : nonterm_semantic list -> spec_func option = function
  | [] ->
      None
  | `SEM_KEEP func :: _ ->
      Some func
  | _ :: tl ->
      keep_of_nonterm tl

let keep_of_nonterm nonterm =
  keep_of_nonterm nonterm.nbase.semantic


let rec classify_of_term : term_semantic list -> spec_func option = function
  | [] ->
      None
  | `SEM_CLASSIFY func :: _ ->
      Some func
  | _ :: tl ->
      classify_of_term tl

let classify_of_term term =
  classify_of_term term.tbase.semantic


let rec dup_of_symbol : semantic list -> spec_func option = function
  | [] ->
      None
  | `SEM_DUP func :: _ ->
      Some func
  | _ :: tl ->
      dup_of_symbol tl

let dup_of_symbol sym =
  dup_of_symbol (sym.semantic :> semantic list)

let dup_of_nonterm nonterm = dup_of_symbol nonterm.nbase
let dup_of_term term = dup_of_symbol term.tbase


let rec del_of_symbol : semantic list -> spec_func option = function
  | [] ->
      None
  | `SEM_DEL func :: _ ->
      Some func
  | _ :: tl ->
      del_of_symbol tl

let del_of_symbol sym =
  del_of_symbol (sym.semantic :> semantic list)

let del_of_nonterm nonterm = del_of_symbol nonterm.nbase
let del_of_term term = del_of_symbol term.tbase


let rec semtype_of_symbol : semantic list -> CamlAst.ctyp option = function
  | [] ->
      None
  | `SEM_TYPE ctyp :: _ ->
      Some ctyp
  | _ :: tl ->
      semtype_of_symbol tl

let semtype_of_symbol sym =
  semtype_of_symbol (sym.semantic :> semantic list)

let semtype_of_nonterm nonterm = semtype_of_symbol nonterm.nbase
let semtype_of_term term = semtype_of_symbol term.tbase
