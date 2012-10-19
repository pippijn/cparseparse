module type ConvertibleType = sig
  include BatSet.OrderedType

  open Sexplib

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end
