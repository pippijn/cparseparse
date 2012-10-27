open Sexplib.Conv


type chr  = string Sloc.t with sexp
type str  = string Sloc.t with sexp
type name = string Sloc.t with sexp
type code = string Sloc.t with sexp


type character =
  | Single of chr             		(* ['a'] *)
  | Range of chr * chr           	(* ['a'-'z'] *)
  with sexp

type char_class =
  | Positive of character list		(* ['a' 'b'] *)
  | Negative of character list		(* [^ 'a' 'b'] *)
  with sexp


type regexp =
  (* atoms *)
  | Eof                                 (* eof *)
  | AnyChar                             (* _ *)
  | Char of chr               		(* 'c' *)
  | String of str             		(* "class" *)
  | Lexeme of name                      (* reference to let-defined lexeme *)
  | AndGrouping of regexp list          (* sub-regexps in parenthesis *)
  | OrGrouping of regexp list		(* sub-regexps separated by "|" *)
  | CharClass of char_class	        (* character class *)
  (* modifiers *)
  | Question of regexp                  (* regexp? *)
  | Star of regexp                      (* regexp* *)
  | Plus of regexp                      (* regexp+ *)
  | Quantified of regexp
                * int option
                * int option            (* regexp{1,5} *)
  (* as-binding *)
  | Binding of regexp * name
  with sexp


type alias =
  | Alias of name * regexp		(* let-defined lexeme *)
  with sexp

type rule =
  | Rule of regexp * code
  with sexp

type lexer =
  | Lexer of name * name list * rule list
  with sexp

type t =
  | Program of code option * alias list * lexer list * code option
  with sexp
