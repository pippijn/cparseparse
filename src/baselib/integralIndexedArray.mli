(* This type is hidden. The type inside the functor is visible and defined in
 * terms of this hidden type, so we can define common operations on all
 * instances of the Make functor.
 *
 * All common operations drop the phantom types. If there is a common
 * operation that does not drop the phantom type, it should be defined inside
 * the functor. *)
type ('a, 'b) repr

val append : ('a, 'b) repr -> ('a, 'c) repr -> 'a array

module Make : functor (T : Sig.IntegralType) -> sig
  include Sig.IntegralIndexedArrayType with
    type integer = T.t
    and type 'a t = ('a, T.t) repr
end
