open Sexplib.Conv

type 'a t = string
and uident = [`U] t
and lident = [`L] t
and 'a ident = ([< `U|`L] as 'a) t

let uident name =
  if BatChar.is_uppercase name.[0] || name.[0] = '_' then
    name
  else
    String.capitalize name

let lident name =
  if BatChar.is_lowercase name.[0] || name.[0] = '_' then
    name
  else
    String.uncapitalize name

let ident name = name

let uident_of_sexp (Sexplib.Type.Atom name) = name
let lident_of_sexp (Sexplib.Type.Atom name) = name
let sexp_of_uident name = (Sexplib.Type.Atom name)
let sexp_of_lident name = (Sexplib.Type.Atom name)

let string_of_lident ident = ident
let string_of_uident ident = ident

let string_of_ident ident = ident

let uident_of_lident ident = uident ident
let lident_of_uident ident = lident ident

let pp_uident pp ident = Format.pp_print_string pp (string_of_uident ident)
let pp_lident pp ident = Format.pp_print_string pp (string_of_lident ident)
let pp_ident pp ident = Format.pp_print_string pp (string_of_ident ident)
