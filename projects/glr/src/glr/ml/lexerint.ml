type 'tok_type lexer = {
  token : unit -> 'tok_type;
  index : 'tok_type -> int;
  sval  : 'tok_type -> SemanticValue.t;
  sloc  : 'tok_type -> SourceLocation.t;
}
