type t

val make_leaf : string -> t
val make : string -> int -> (int -> t) -> t

val add_alternative : t -> t -> unit

val print_tree : t -> out_channel -> bool -> unit
