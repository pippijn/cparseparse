module Set = struct
  open Sig

  type 'mutability t

  (* the approach using BatBitSet turned out to be too much of a
   * bottleneck, so now the bitsets are implemented in C++.
   * they are comparable, hashable and serialisable and clean
   * up their memory on garbage collection *)
  external create        : int -> 'm t      		= "ml_BitSet_create"
  external copy          : 'm t -> 'n t   		= "ml_BitSet_copy"
  external cardinal      : 'm t -> int         		= "ml_BitSet_count"
  external add           : writable t -> int -> unit 	= "ml_BitSet_set"
  external mem           : 'm t -> int -> bool 		= "ml_BitSet_is_set"
  external unite         : writable t -> 'm t -> unit   = "ml_BitSet_unite"
  external differentiate : writable t -> 'm t -> unit   = "ml_BitSet_differentiate"
  external merge         : writable t -> 'm t -> bool   = "ml_BitSet_merge"
  external clear         : writable t -> unit        	= "ml_BitSet_clear"
  external assign        : writable t -> 'm t -> unit   = "ml_BitSet_assign"

  external readonly	 : writable t -> readonly t	= "%identity"

  let empty : readonly t = create 0

  let t_of_sexp () sexp =
    empty

  let sexp_of_t () bset =
    Sexplib.Sexp.List []
end


module Make(T : Sig.IntegralType) = struct
  include Set

  let create n = create (T.to_int n + 1)
  let add s v = add s (T.to_int v)
  let mem s v = mem s (T.to_int v)
end
