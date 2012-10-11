(*----------------------------------------------------------------------------
        C0 AST - lousy AST before the symbol and ambiguity resolution
  --------------------------------------------------------------------------*)
open Sexplib.Conv

(* Toplevel definition *)
type program = topl
and topl =
    ToNil
  | ToDef of Loc.t * def * topl
  | ToDecl of Loc.t * decl * topl
  | ToTypedef of Loc.t * Ident.t * typ
  (* | ToStruct of Loc.t * struc *)
(* Type *)
and typ =
    TyId of Loc.t * Ident.t
  | TySta of Loc.t * typ
  | TyAmp of Loc.t * typ
  | TyAnn of Loc.t * ann * typ
(* Generic annotation *)
and ann =
    AnNil
  | AnId of Loc.t * Ident.t * ann
  | AnSta of Loc.t * ann
  | AnAmp of Loc.t * ann
(* Declaration *)
and decl =
  | DcVar of Loc.t * typ * ann * Ident.t
  | DcFun of Loc.t * typ * ann * Ident.t * targ
and def =
  | DfVar of Loc.t * typ * ann * Ident.t * expr
  | DfFun of Loc.t * typ * ann * Ident.t * targ * block
(* Typed argument *)
and targ =
    TaNil
  | TaItem of Loc.t * typ * Ident.t option * targ
(* Expression *)
and expr =
  | ExApp of Loc.t * Ident.t * arg
  | ExOp of Loc.t * expr * Ident.t * expr
  | ExId of Loc.t * Ident.t
  | ExLit of Loc.t * Lit.t
  | ExDot of Loc.t * expr * Ident.t
  | ExArr of Loc.t * expr * Ident.t
  | ExPar of Loc.t * expr
  | ExAmp of Loc.t * expr
  | ExSta of Loc.t * expr
  | ExCom of Loc.t * expr * expr
(* Statement *)
and stmt =
    (* StAss of Loc.t * lval * Ident.t * expr *)
  | StEx of Loc.t * expr
  | StBlock of Loc.t * block
  | StDo of Loc.t * expr * block
  | StWhile of Loc.t * block * expr
  | StFor of expr * expr * expr * block
  | StTry of block * targ * block
  | StCom of Loc.t * expr * expr
(* Block *)
and block =
    BlNil
  | BlSta of Loc.t * stmt
(* Argument *)
and arg =
    ArNil
  | ArId of Loc.t * Ident.t * arg with sexp
and qual = `Const | `Volatile | `Restrict
let output_program channel s = Sexplib.Sexp.output_hum channel (sexp_of_topl s)
