open GrammarType

(************************************************************
 * :: Common Operations
 ************************************************************)


module M : S with type t = terminal = struct

  type t = terminal

  let hash a =
    a.term_index

  let compare a b =
    a.term_index - b.term_index

  let equal a b =
    a.term_index = b.term_index

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_terminal
  let t_of_sexp = terminal_of_sexp

  let default = empty_terminal

end

module Table = Hashtbl.Make(M)
module Map = SexpMap.Make(M)
module Set = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
