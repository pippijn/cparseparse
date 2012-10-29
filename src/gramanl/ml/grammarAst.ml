open Sexplib.Conv


(* one of the things that appears after the '->', i.e. the right-hand-side elements *)
type rhs =
  | RH_name of (* tag: *)string Sloc.t option * (* name: *)string Sloc.t (* tagged terminal or nonterminal reference *)
  | RH_string of (* tag: *)string Sloc.t option * (* str: *)string Sloc.t (* tagged terminal reference by string alias *)
  | RH_prec of (* token: *)string Sloc.t (* assigns a specific precedence level to a rule *)
  | RH_forbid of (* token: *)string Sloc.t (* forbids a rule from being used when a given token is next in the input *)
  with sexp

type proddecl_kind =
  | PDK_NEW (* new production (most productions are new) *)
  | PDK_DELETE (* means to delete the production from the base spec *)
  | PDK_REPLACE (* replace original production with this one *)
  with sexp

(* production: rhs description, and code to execute upon recognition *)
type proddecl =
  | ProdDecl of (* kind: *)proddecl_kind * (* name: *)string Sloc.t option * (* rhs: *)rhs list * (* action: *)string Sloc.t option
  with sexp

(* specification function: formals + code *)
type specfunc =
  | SpecFunc of (* name: *)string Sloc.t * (* formals: *)string Sloc.t list * (* code: *)string Sloc.t
  with sexp

(* a set of equal-precedence tokens (named either by name or by alias), 
 * and the 'kind' which tells how they associate; 'prec' is interpreted 
 * such that larger values have higher precedence *)
type precspec =
  | PrecSpec of (* kind: *)Assoc.kind Sloc.t * (* prec: *)int * (* tokens: *)string Sloc.t list
  with sexp

(* token with lexer code 'code' and grammar name 'name', with grammar alias 'alias' *)
type termdecl =
  | TermDecl of (* code: *)int * (* name: *)string Sloc.t * (* alias: *)string Sloc.t option
  with sexp

(* declaration that token with grammar name 'name' has semantic values with OCaml type 'type' *)
type termtype =
  | TermType of (* name: *)string Sloc.t * (* type: *)string Sloc.t * (* funcs: *)specfunc list
  with sexp

(* toplevel form *)
type topform =
  (* arbitrary verbatim section, emitted into either the interface 
   * (isImpl=false) or implementation (isImpl=true) file *)
  | TF_verbatim of (* isImpl: *)bool * (* code: *)string Sloc.t
  (* declarative option; semantics vary based on 'name' *)
  | TF_option of (* name: *)string Sloc.t * (* value: *)int
  (* definition of tokens *)
  | TF_terminals of (* decls: *)termdecl list (* ids and aliases *)
  		  * (* types: *)termtype list (* type annotations *)
                  * (* precs: *)precspec list (* precedence and associativity *)
  (* a nonterminal, with productions *)
  | TF_nonterm of (* name: *)string Sloc.t (* nonterm name *)
  		* (* type: *)string Sloc.t option (* semantic value type *)
                * (* funcs: *)specfunc list (* special situation action functions *)
                * (* prods: *)proddecl list (* productions (right-hand side alternatives) *)
                * (* subsets: *)string Sloc.t list (* preference subset nonterminals (for scannerless) *)
  with sexp

type topforms = topform list with sexp