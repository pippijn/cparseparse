module Make : functor (T : Sig.IntegralType) -> sig
  open Sig

  type integer = T.t
  type 'mutability t

  val create : integer -> integer -> 'm t
  val empty : readonly t

  val set : writable t -> integer -> integer -> unit
  val is_set : 'm t -> integer -> integer -> bool
  val test_and_set : writable t -> integer -> integer -> bool

  val print : 'm t -> unit
  val readonly : writable t -> readonly t

  val t_of_sexp : (Sexplib.Sexp.t -> 'm) -> Sexplib.Sexp.t -> 'm t
  val sexp_of_t : ('m -> Sexplib.Sexp.t) -> 'm t -> Sexplib.Sexp.t
end
