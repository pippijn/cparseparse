open Sexplib

module Make(T : Sig.OrderedConvertibleType)
= SexpMap.Make(struct

    type t = T.t Sloc.t

    let compare = Sloc.compare ~cmp:T.compare
    
    let t_of_sexp = Sloc.t_of_sexp T.t_of_sexp
    let sexp_of_t = Sloc.sexp_of_t T.sexp_of_t

  end)
