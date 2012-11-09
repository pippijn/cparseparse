module type S = SexpMap.S with type key = SexpString.t

include SexpMap.Make(SexpString)
