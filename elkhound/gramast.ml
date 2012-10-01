open Sexplib.Conv


type rhs =
  | RH_name of string * string
  | RH_string of string * string
  | RH_prec of string
  | RH_forbid of string
  with sexp

type proddecl_kind =
  | PDK_NEW
  | PDK_DELETE
  | PDK_REPLACE
  with sexp

type proddecl =
  | ProdDecl of proddecl_kind * rhs list * string
  with sexp

type specfunc =
  | SpecFunc of string * string list * string
  with sexp

type precspec =
  | PrecSpec of Assoc.kind * int * string list
  with sexp

type termdecl =
  | TermDecl of int * string * string
  with sexp

type termtype =
  | TermType of string * string * specfunc list
  with sexp

type topform =
  | TF_verbatim of bool * string
  | TF_option of string * int
  | TF_terminals of termdecl list * termtype list * precspec list
  | TF_nonterm of string * string * specfunc list * proddecl list * string list
  with sexp

type topforms = topform list with sexp
