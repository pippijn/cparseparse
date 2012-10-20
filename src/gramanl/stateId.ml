open Sexplib.Conv

type t = int with sexp

let to_int (id : t) : int = id
let of_int (id : int) : t = id
