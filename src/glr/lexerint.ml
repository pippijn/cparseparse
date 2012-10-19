module type TokenInfo = sig
  type t

  val name : t -> string
  val desc : t -> string
  val index : t -> int
  val sval : t -> SemanticValue.t
end


type 'tok_type lexer = {
  token : unit -> 'tok_type;
  index : 'tok_type -> int;
  sval  : 'tok_type -> SemanticValue.t;
  sloc  : 'tok_type -> SourceLocation.t;
}
