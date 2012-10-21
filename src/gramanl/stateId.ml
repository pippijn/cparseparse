open Sexplib.Conv

let (--) = BatPervasives.(--)


module IntegralModule = struct
  type t = int with sexp

  let compare (a : t) (b : t) : int = a - b
  let hash : t -> int = BatPervasives.identity
  let equal : t -> t -> bool = (==)

  let to_int : t -> int = BatPervasives.identity
  let of_int : int -> t = BatPervasives.identity

  let range s e = s -- e

  let to_string (id : t) = string_of_int id
  let print out (id : t) = output_string out (to_string id)
  let sprint () = to_string
end


module State = struct
  include IntegralModule

  let default : t = -1

  let start : t = 0
  let is_start (id : t) = id = start
end


(* nonterminals *)
module Nonterminal = struct
  include IntegralModule

  let empty : t = 0
  let is_empty (index : t) = index = empty

  let start : t = 1
  let is_start (index : t) = index = start

  let default = empty
end


(* terminals *)
module Terminal = struct
  include IntegralModule

  let eof = 0
  let is_eof (index : t) = index = eof

  let default : t = -1
end
