exception ParseError of ParseTablesType.state_id * int
exception Located of SourceLocation.t * exn * string
val cancel : string -> 'a

type statistics = {
  mutable numStackNodesAllocd : int;
  mutable maxStackNodesAllocd : int;
  mutable detShift : int;
  mutable detReduce : int;
  mutable nondetShift : int;
  mutable nondetReduce : int;
}

type 'result glr

val stats_of_glr : 'a glr -> statistics

val makeGLR : 'result UserActions.t -> ParseTablesType.t -> 'result glr
val glrParse : 'result glr -> 'token Lexerint.lexer -> 'result
