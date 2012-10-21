type ('a, 'b) repr

val append : ('a, 'b) repr -> ('a, 'c) repr -> 'a array

module Make : functor (T : Sig.IntegralType) -> sig
  include Sig.IntegralIndexedArrayType with
    type integer = T.t
    and type 'a t = ('a, T.t) repr
end
