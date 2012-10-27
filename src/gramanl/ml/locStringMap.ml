open Sexplib

include SexpMap.Make(struct

  type t = string Sloc.t

  let compare = Sloc.compare ~cmp:String.compare
  
  let t_of_sexp = Sloc.t_of_sexp Conv.string_of_sexp
  let sexp_of_t = Sloc.sexp_of_t Conv.sexp_of_string

end)
