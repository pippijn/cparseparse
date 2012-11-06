(* This type is hidden. The type inside the functor is visible and defined in
 * terms of this hidden type, so we can define common operations on all
 * instances of the Make functor.
 *
 * All common operations drop the phantom types. If there is a common
 * operation that does not drop the phantom type, it should be defined inside
 * the functor. *)
type ('a, 'mutability, 'integer) repr

val append : ('a, 'mut_a, 'int_a) repr -> ('a, 'mut_b, 'int_b) repr -> 'a array

module Make : functor (T : Sig.IntegralType) -> sig
  include Sig.DenseIntMapType with
    type integer = T.t
    and type ('a, 'm) t = ('a, 'm, T.t) repr
end

include Sig.DenseIntMapType with
  type integer = int
  and type ('a, 'm) t = ('a, 'm, int) repr
