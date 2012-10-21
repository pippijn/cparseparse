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


module CamlSyntax = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax))

let parse_string syntax default _loc str =
  match str with
  | ""  -> default
  | str ->
      try
        CamlSyntax.Gram.parse_string syntax _loc str
      with Loc.Exc_located (loc, Stream.Error msg) ->
        Format.fprintf Format.str_formatter "%a\n  while parsing \"%s\": %s" Loc.print loc str msg;
        Diagnostics.error (Format.flush_str_formatter ())

let ctyp_of_string      _loc = parse_string CamlSyntax.ctyp      <:ctyp<unit>> _loc
let expr_of_string      _loc = parse_string CamlSyntax.expr      <:expr<()>>   _loc
let sig_items_of_string _loc = parse_string CamlSyntax.sig_items <:sig_item<>> _loc
let str_items_of_string _loc = parse_string CamlSyntax.str_items <:str_item<>> _loc
