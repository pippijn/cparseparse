module type S = sig
  type t

  include Hashtbl.HashedType            with type t := t
  include Map.OrderedType               with type t := t
  include Sig.ConvertibleType           with type t := t
  include Graph.Sig.COMPARABLE          with type t := t
  include Graph.Sig.ORDERED_TYPE_DFT    with type t := t
end
