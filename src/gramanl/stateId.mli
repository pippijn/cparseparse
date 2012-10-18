include Sig.OrderedConvertibleType

val to_int : t -> int
val of_int : int -> t

val to_string : t -> string
val print : out_channel -> t -> unit
val sprint : unit -> t -> string

val default : t

val is_start : t -> bool
