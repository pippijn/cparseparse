open Ast


let is_type_keyword = function
  | `UM_WCHAR_T
  | `UM_BOOL
  | `UM_SHORT
  | `UM_INT
  | `UM_LONG
  | `UM_SIGNED
  | `UM_UNSIGNED
  | `UM_FLOAT
  | `UM_DOUBLE
  | `UM_VOID
  | `UM_LONG_LONG
  | `UM_CHAR
  | `UM_COMPLEX
  | `UM_IMAGINARY ->
      true
  | _ ->
      false


let dflags_of_modifiers mods =
  let rec filter flags mods =
    match mods with
    | [] ->
        flags
    |(`UM_AUTO
    | `UM_REGISTER
    | `UM_STATIC
    | `UM_EXTERN
    | `UM_MUTABLE
    | `UM_INLINE
    | `UM_VIRTUAL
    | `UM_EXPLICIT
    | `UM_FRIEND
    | `UM_TYPEDEF as hd) :: tl ->
        filter (hd :: flags) tl
    | hd :: tl ->
        filter flags tl
  in

  filter [] mods


let cv_of_modifiers mods =
  let rec filter flags mods =
    match mods with
    | [] ->
        flags
    |(`UM_CONST
    | `UM_VOLATILE
    | `UM_RESTRICT as hd) :: tl ->
        filter (hd :: flags) tl
    | hd :: tl ->
        filter flags tl
  in

  filter [] mods


let stype_of_modifiers mods =
  (* implement cppstd Table 7, p.109 *)
  match List.sort compare (List.filter is_type_keyword mods) with
  | [`UM_CHAR] -> ST_Char
  | [`UM_INT] -> ST_SInt
  | [`UM_LONG; `UM_INT] -> ST_SLong
  | [`UM_UNSIGNED; `UM_LONG; `UM_INT] -> ST_ULong
  | mods -> failwith ("malformed type: " ^ Sexplib.Sexp.to_string_hum (sexp_of_modifiers mods))


(* TODO: check whether ocv is ever something other than [] *)
let set_cv cv = function
  | TS_name (ocv, name, typenameUsed) -> TS_name (ocv @ cv, name, typenameUsed)
  | TS_simple (ocv, tid) -> TS_simple (ocv @ cv, tid)
  | TS_elaborated (ocv, tint, name) -> TS_elaborated (ocv @ cv, tint, name)
  | TS_classSpec (ocv, tint, name, base, mems) -> TS_classSpec (ocv @ cv, tint, name, base, mems)
  | TS_enumSpec (ocv, name, enums) -> TS_enumSpec (ocv @ cv, name, enums)
