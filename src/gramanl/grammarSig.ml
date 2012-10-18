module type S = sig
  type t

  include Hashtbl.HashedType            with type t := t
  include Map.OrderedType               with type t := t
  include Sig.ConvertibleType           with type t := t
  include Graph.Sig.COMPARABLE          with type t := t
  include Graph.Sig.ORDERED_TYPE_DFT    with type t := t
end


module type IntegralModuleType = sig
  type t

  include Sig.OrderedConvertibleType	with type t := t
  include Sig.IntegralType		with type t := t

  val hash : t -> int
  val equal : t -> t -> bool

  val to_string : t -> string
  val print : out_channel -> t -> unit
  val sprint : unit -> t -> string

  val default : t
end
