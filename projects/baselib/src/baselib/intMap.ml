open Sexplib

include SexpMap.Make(struct
  include BatInt
  
  let t_of_sexp = Conv.int_of_sexp
  let sexp_of_t = Conv.sexp_of_int

end)
