open Camlp4.PreCast

type ctyp = Ast.ctyp
type expr = Ast.expr
type patt = Ast.patt
type sig_item = Ast.sig_item
type str_item = Ast.str_item

let ctyp_of_sexp     sx = Ast.TyNil (Loc.ghost)
let expr_of_sexp     sx = Ast.ExNil (Loc.ghost)
let patt_of_sexp     sx = Ast.PaNil (Loc.ghost)
let sig_item_of_sexp sx = Ast.SgNil (Loc.ghost)
let str_item_of_sexp sx = Ast.StNil (Loc.ghost)

let sexp_of_ctyp     ty = Sexplib.Sexp.List []
let sexp_of_expr     ex = Sexplib.Sexp.List []
let sexp_of_patt     pa = Sexplib.Sexp.List []
let sexp_of_sig_item sg = Sexplib.Sexp.List []
let sexp_of_str_item st = Sexplib.Sexp.List []
