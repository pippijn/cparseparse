open Sexplib.Conv

type t = int with sexp

let compare (a : t) (b : t) : int = a - b

let to_int (id : t) : int = id
let of_int (id : int) : t = id

let to_string (id : t) = string_of_int id
let print out (id : t) = output_string out (to_string id)
let sprint () = to_string

let default : t = -1

let is_start (id : t) = id = 0
