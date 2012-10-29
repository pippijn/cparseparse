open Sexplib

include SexpMap.Make(struct
  type t = char

  let compare a b = Char.code a - Char.code b

  let t_of_sexp = Conv.char_of_sexp
  let sexp_of_t = Conv.sexp_of_char

end)
