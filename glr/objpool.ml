(* objpool.ml *)
(* pool of allocated objects for explicit re-use *)
                                  
(* This object pool maintains a set of objects that are available
 * for use.  It must be given a way to create new objects. *)
type 'a t = {
  allocFunc : unit -> 'a;

  (* implementation is just an array of elements that have been made
   * available for re-use; this should be regarded as private
   * inheritance, though I don't see how to do that in OCaml *)
  stack : 'a Arraystack.t;
}

let make allocFunc = {
  allocFunc;
  stack = Arraystack.make (allocFunc ());
}

(* retrieve an object ready to be used; might return a pool element,
 * or if the pool is empty, will make a new element *)
let alloc { allocFunc; stack } =
  if Arraystack.isNotEmpty stack then
    (* just grab the topmost element in the pool stack *)
    Arraystack.pop stack
  else
    (* make a new object; I thought about making several at a time
     * but there seems little advantage.. *)
    allocFunc ()

(* return an object to the pool so it can be re-used *)
let dealloc { stack } obj =
  (* put the element into the stack *)
  Arraystack.push stack obj


(* EOF *)
