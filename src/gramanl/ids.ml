open Sexplib.Conv

let (--) = BatPervasives.(--)


module IntegralModule = struct
  type t = int with sexp

  let compare (a : t) (b : t) : int = a - b
  let hash : t -> int = BatPervasives.identity
  let equal : t -> t -> bool = (==)

  let to_int : t -> int = BatPervasives.identity
  let of_int : int -> t = BatPervasives.identity

  let domain e = 0 -- e


  let rec find f i e =
    if i <= e then
      if f i then
        i
      else
        find f (i + 1) e
    else
      raise Not_found

  let find f e =
    find f 0 e


  let rec fold_left f x i e =
    if i <= e then
      fold_left f (f x i) (i + 1) e
    else
      x

  let fold_left f x e =
    fold_left f x 0 e


  let iter f e =
    for i = 0 to e do
      f i
    done


  let to_string (id : t) = string_of_int id
  let print out (id : t) = output_string out (to_string id)
  let sprint () = to_string
end


module State = struct
  include IntegralModule

  let start : t = 0
  let is_start (id : t) = id = start

  let default : t = -1
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


(* productions *)
module Production = struct
  include IntegralModule

  let start : t = 0
  let is_start (id : t) = id = start

  let default : t = -1
end
