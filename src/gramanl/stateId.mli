type t

val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

val to_int : t -> int
val of_int : int -> t
