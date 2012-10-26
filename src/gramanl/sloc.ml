type position = Lexing.position
let position_of_sexp sx = Lexing.dummy_pos
let sexp_of_position po = Sexplib.Sexp.List []

let dummy_pos = Lexing.dummy_pos
let generated_pos = Lexing.({ dummy_pos with pos_fname = "<generated>" })


type 'a t =
  'a * position * position
  with sexp

let value   (t, s, e) = t
let start_p (t, s, e) = s
let end_p   (t, s, e) = e

let dummy t =
  t, dummy_pos, dummy_pos

let generated t =
  t, generated_pos, generated_pos


let _loc (t, s, e) =
  let open Lexing in

  Camlp4.PreCast.Loc.of_tuple (
    (* file name *)
    s.pos_fname,

    (* start *)
    s.pos_lnum,
    s.pos_bol,
    s.pos_cnum,

    (* end *)
    e.pos_lnum,
    e.pos_bol,
    e.pos_cnum,

    false
  )

let lid (t, s, e) =
  let _loc = _loc (t, s, e) in
  Camlp4.PreCast.Ast.(ExId (_loc, IdLid (_loc, t)))

let uid (t, s, e) =
  let _loc = _loc (t, s, e) in
  Camlp4.PreCast.Ast.(ExId (_loc, IdUid (_loc, t)))
