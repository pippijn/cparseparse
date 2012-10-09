(*----------------------------------------------------------------------------
        C0 AST - lousy AST before the symbol and ambiguity resolution
  --------------------------------------------------------------------------*)

(* Toplevel definition *)
type 'a topl =
    ToNil
  | ToDef of 'a * 'a def * 'a topl
  | ToDecl of 'a * 'a decl * 'a topl
  | ToTypedef of 'a * 'a Ident.t * 'a typ
  | ToStruct of 'a * 'a struc
(* Type *)
and 'a typ =
    TyId of 'a * 'a Ident.t
  | TySta of 'a * 'a typ
  | TyAmp of 'a * 'a typ
  | TyAnn of 'a * 'a ann * 'a typ
(* Generic annotation *)
and 'a ann =
    AnNil
  | AnId of 'a * 'a Ident.t * 'a ann
(* Declaration *)
and 'a decl =
  | DcVar of 'a * 'a typ * 'a ann * 'a Ident.t
  | DcFun of 'a * 'a typ * 'a ann * 'a Ident.t * 'a targ
and 'a def =
  | DfVar of 'a * 'a typ * 'a ann * 'a Ident.t * 'a expr
  | DfFun of 'a * 'a typ * 'a ann * 'a Ident.t * 'a targ * 'a block
(* Typed argument *)
and 'a targ =
    TaNil
  | TaItem of 'a * 'a typ * 'a Ident.t option * 'a targ
(* Expression *)
and 'a expr =
  | ExApp of 'a * 'a Ident.t * 'a arg
  | ExOp of 'a * 'a expr * 'a Ident.t * 'a expr
  | ExId of 'a * 'a Ident.t
  | ExLit of 'a * 'a Lit.t
  | ExDot of 'a * 'a expr * Ident.t
  | ExArr of 'a * 'a expr * Ident.t
  | ExPar of 'a * 'a expr
  | ExAmp of 'a * 'a expr
  | ExSta of 'a * 'a expr
  | ExCom of 'a * 'a expr * 'a expr
(* Statement *)
and 'a stmt =
    StAss of 'a * 'a lval * 'a Ident.t * 'a expr
  | StEx of 'a * 'a expr
  | StBlock of 'a * 'a block
  | StDo of 'a * 'a expr * 'a block
  | StWhile of 'a * 'a block * 'a expr
  | StFor of 'a expr * 'a expr * 'a expr * 'a block
  | StTry of 'a block * 'a targ * 'a block
  | StCom of 'a * 'a expr * 'a expr
(* Block *)
and 'a block =
    BlNil
  | BlSta of 'a * 'a stmt
(* Argument *)
and 'a arg =
    ArNil
  | ArId of 'a * 'a Ident.t * 'a arg
