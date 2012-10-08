(* secret to type casting in OCaml: the Obj module *)
type t = Obj.t

let repr = Obj.repr
let obj = Obj.obj

let null = repr ()
