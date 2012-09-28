type t = {
  mutable tokType : int;
  mutable sval : Useract.tSemanticValue;
}


(* interface that must be implemented *)
module type Interface = sig
  (* yield a description of an arbitrary token kind *)
  val tokenKindDesc : int -> string
end
