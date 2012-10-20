module type ConvertibleType = sig
  open Sexplib

  type t

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end


module type OrderedConvertibleType = sig
  type t

  include BatInterfaces.OrderedType with type t := t
  include ConvertibleType with type t := t
end
