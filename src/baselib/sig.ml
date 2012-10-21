module type ConvertibleType = sig
  open Sexplib

  type t

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end


module type OrderedConvertibleType = sig
  type t

  include BatInterfaces.OrderedType with type t := t
  include ConvertibleType with type t := t
end


module type IntegralType = sig
  type t

  val to_int : t -> int
  val of_int : int -> t

  val range : int -> int -> t BatEnum.t
end


module type IntegralIndexedArrayType = sig
  type integer
  type 'a t

  val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t

  val to_array : 'a t -> 'a array
  val to_list : 'a t -> 'a list

  val get : 'a t -> integer -> 'a
  val set : 'a t -> integer -> 'a -> unit

  val length : 'a t -> int
  val range : 'a t -> integer BatEnum.t

  val empty : 'a t
  val make : int -> 'a -> 'a t
  val init : int -> (integer -> 'a) -> 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
  val foldl_until : ('a -> int) -> int -> 'a t -> int
  val foldl_untili : (integer -> 'a -> int) -> int -> 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (integer -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (integer -> 'a -> 'b) -> 'a t -> 'b t
  val find : ('a -> bool) -> 'a t -> 'a
  val exists : ('a -> bool) -> 'a t -> bool

  val sum : ('a -> int) -> 'a t -> int
  val count : ('a -> bool) -> 'a t -> int
end
