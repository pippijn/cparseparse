type t = {
  mutable tokType : int;
  mutable sval : Useract.tSemanticValue;
}

type ('lexbuf, 'token) lexer = {
  from_channel : in_channel -> 'lexbuf;
  lexeme : 'lexbuf -> string;
  lexeme_start : 'lexbuf -> int;

  token : 'lexbuf -> 'token;
  line : int ref;
}
