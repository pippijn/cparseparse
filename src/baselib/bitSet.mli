(* Internal implementation module. Should not be used by client code.
 * This is only exposed so Bit2d can use it. *)
module Internal : sig
  open Sig

  type 'mutability t

  external create        : int -> 'm t      		= "ml_BitSet_create"
  external add           : int -> writable t -> unit 	= "ml_BitSet_add"
  external mem           : int -> 'm t -> bool 		= "ml_BitSet_mem"

  val t_of_sexp	: unit -> 'a -> 'b t
  val sexp_of_t	: unit -> 'a -> Sexplib.Sexp.t
end


module Make : functor (T : Sig.IntegralType) -> sig
  open Sig

  type 'mutability t

  val create	: T.t -> 'm t
  val cardinal	: 'm t -> int
  val add	: T.t -> writable t -> unit
  val mem	: T.t -> 'm t -> bool

  val readonly	: writable t -> readonly t
end
