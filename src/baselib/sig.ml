type readonly
type writable

let readonly_of_sexp = ()
let sexp_of_readonly = ()
let writable_of_sexp = ()
let sexp_of_writable = ()


module type ConvertibleType = sig
  open Sexplib

  type t

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end


module type OrderedConvertibleType = sig
  type t

  include Map.OrderedType with type t := t
  include ConvertibleType with type t := t
end


module type HashedConvertibleType = sig
  type t

  include Hashtbl.HashedType with type t := t
  include ConvertibleType with type t := t
end


module type IntegralType = sig
  type t

  val to_int : t -> int
  val of_int : int -> t
end


module type IntegralModuleType = sig
  type t

  include OrderedConvertibleType	with type t := t
  include IntegralType			with type t := t

  val hash : t -> int
  val equal : t -> t -> bool

  val to_string : t -> string
  val print : out_channel -> t -> unit
  val sprint : unit -> t -> string

  val find : (t -> bool) -> t -> t
  val fold_left : ('a -> t -> 'a) -> 'a -> t -> 'a
  val iter : (t -> unit) -> t -> unit

  val default : t
end


module type DenseIntMapType = sig
  type integer
  type ('a, 'mutability) t

  val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> unit -> Sexplib.Sexp.t -> ('a, 'm) t
  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> unit -> ('a, 'm) t -> Sexplib.Sexp.t

  val readonly : ('a, writable) t -> ('a, readonly) t

  val to_array : ('a, 'm) t -> 'a array
  val to_list : ('a, 'm) t -> 'a list

  val get : ('a, 'm) t -> integer -> 'a
  val set : ('a, writable) t -> integer -> 'a -> unit

  val length : ('a, 'm) t -> int
  val last_index : ('a, 'm) t -> integer

  val empty : ('a, 'm) t
  val make : int -> 'a -> ('a, 'm) t
  val init : int -> (integer -> 'a) -> ('a, readonly) t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, 'm) t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> ('b, 'm) t -> 'a -> 'a
  val foldl_until : ('a -> int) -> int -> ('a, 'm) t -> int
  val foldl_untili : (integer -> 'a -> int) -> int -> ('a, 'm) t -> int
  val iter : ('a -> unit) -> ('a, 'm) t -> unit
  val iteri : (integer -> 'a -> unit) -> ('a, 'm) t -> unit
  val map : ('a -> 'b) -> ('a, 'm) t -> ('b, readonly) t
  val mapi : (integer -> 'a -> 'b) -> ('a, 'm) t -> ('b, readonly) t
  val find : ('a -> bool) -> ('a, 'm) t -> 'a
  val exists : ('a -> bool) -> ('a, 'm) t -> bool
  val mem : 'a -> ('a, 'm) t -> bool
  val memq : 'a -> ('a, 'm) t -> bool

  val sum : ('a -> int) -> ('a, 'm) t -> int
  val count : ('a -> bool) -> ('a, 'm) t -> int
end
