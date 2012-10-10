open Sexplib

include SexpMap.Make(struct
  include String
  
  let t_of_sexp = Conv.string_of_sexp
  let sexp_of_t = Conv.sexp_of_string

end)
