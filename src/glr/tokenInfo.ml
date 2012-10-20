module type S = sig
  type t

  val name : t -> string
  val desc : t -> string
  val index : t -> int
  val sval : t -> SemanticValue.t
end
