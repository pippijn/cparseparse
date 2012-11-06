module Internal = struct
  open Sig

  type 'mutability t

  (* the approach using BatBitSet turned out to be too much of a
   * bottleneck, so now the bitsets are implemented in C++.
   * they are comparable, hashable and serialisable and clean
   * up their memory on garbage collection *)
  external create        : int -> 'm t      		= "ml_BitSet_create"
  external cardinal      : 'm t -> int         		= "ml_BitSet_cardinal"
  external add           : int -> writable t -> unit 	= "ml_BitSet_add"
  external mem           : int -> 'm t -> bool 		= "ml_BitSet_mem"

  external readonly	 : writable t -> readonly t	= "%identity"

  let t_of_sexp () sexp =
    create 0

  let sexp_of_t () bset =
    Sexplib.Sexp.Atom "<BitSet>"
end


module Make(T : Sig.IntegralType) = struct
  type 'mutability t = 'mutability Internal.t

  let create n = Internal.create (T.to_int n + 1)
  let cardinal = Internal.cardinal
  let add v = Internal.add (T.to_int v)
  let mem v = Internal.mem (T.to_int v)

  let readonly = Internal.readonly
end
