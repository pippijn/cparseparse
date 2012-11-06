type t = char

let compare a b = Char.code a - Char.code b

let t_of_sexp = Sexplib.Conv.char_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_char
