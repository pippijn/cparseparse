module type TokenInfo = sig
  type t

  val name : t -> string
  val desc : t -> string
  val index : t -> int
  val sval : t -> SemanticValue.t
end


type t = {
  mutable tokType : int;
  mutable tokSval : SemanticValue.t;
}

type ('lexbuf, 'token) lexer = {
  from_channel : in_channel -> 'lexbuf;
  lexeme : 'lexbuf -> string;
  lexeme_start : 'lexbuf -> int;

  token : 'lexbuf -> 'token;
  line : int ref;
}
