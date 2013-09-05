open Ast


let types_of_modifiers mods : type_flags =
  let rec filter_types flags mods =
    match mods with
    | [] -> flags
    |(`UM_WCHAR_T
    | `UM_BOOL
    | `UM_SHORT
    | `UM_INT
    | `UM_LONG
    | `UM_SIGNED
    | `UM_UNSIGNED
    | `UM_FLOAT
    | `UM_DOUBLE
    | `UM_VOID
    | `UM_CHAR
    | `UM_COMPLEX
    | `UM_IMAGINARY as hd) :: tl ->
        filter_types (hd :: flags) tl
    | hd :: tl ->
        filter_types flags tl
  in

  filter_types [] mods


let dflags_of_modifiers mods : decl_flags =
  let rec filter_dflags flags mods =
    match mods with
    | [] -> flags
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
        filter_dflags (hd :: flags) tl
    | hd :: tl ->
        filter_dflags flags tl
  in

  filter_dflags [] mods


let cv_of_modifiers mods : cv_flags =
  let rec filter_cv flags mods =
    match mods with
    | [] -> flags
    |(`UM_CONST
    | `UM_VOLATILE
    | `UM_RESTRICT as hd) :: tl ->
        filter_cv (hd :: flags) tl
    | hd :: tl ->
        filter_cv flags tl
  in

  filter_cv [] mods


let modifier_compare a b =
  let type_order = function
    (* Unmodifiable types. *)
    | `UM_VOID		->  0
    | `UM_BOOL		->  1
    | `UM_WCHAR_T	->  2

    (* Modifiable types. *)
    | `UM_CHAR		->  3
    | `UM_INT		->  4
    | `UM_FLOAT		->  5
    | `UM_DOUBLE	->  6

    (* Modifiers for int/double. *)
    | `UM_SHORT		->  7
    | `UM_LONG		->  8

    (* Signedness for char/int. *)
    | `UM_SIGNED	->  9
    | `UM_UNSIGNED	-> 10

    (* Modifiers for char/int/float/double. *)
    | `UM_COMPLEX	-> 11
    | `UM_IMAGINARY	-> 12
  in

  compare
    (type_order a)
    (type_order b)


let stype_of_modifiers mods =
  (* implement cppstd Table 7, p.109 *)
  match List.sort modifier_compare (types_of_modifiers mods) with
  (* Base type		Modifier		Signedness *)
  | [`UM_VOID;							] -> ST_Void

  | [`UM_BOOL;							] -> ST_Bool
  | [`UM_WCHAR_T;						] -> ST_WCharT

  | [`UM_FLOAT;							] -> ST_Float
  | [`UM_DOUBLE							] -> ST_Double
  | [`UM_DOUBLE;	`UM_LONG;				] -> ST_LDouble

  | [`UM_CHAR;							] -> ST_Char
  | [`UM_CHAR;					`UM_SIGNED;	] -> ST_SChar
  | [`UM_CHAR;					`UM_UNSIGNED;	] -> ST_UChar

  | [`UM_INT;							]
  | [						`UM_SIGNED;	]
  | [`UM_INT;					`UM_SIGNED;	] -> ST_SInt
  | [						`UM_UNSIGNED;	]
  | [`UM_INT;					`UM_UNSIGNED;	] -> ST_UInt

  | [			`UM_SHORT;				]
  | [`UM_INT;		`UM_SHORT;				]
  | [			`UM_SHORT;		`UM_SIGNED;	]
  | [`UM_INT;		`UM_SHORT;		`UM_SIGNED;	] -> ST_SShort
  | [			`UM_SHORT;		`UM_UNSIGNED;	]
  | [`UM_INT;		`UM_SHORT;		`UM_UNSIGNED;	] -> ST_UShort

  | [			`UM_LONG;				]
  | [`UM_INT;		`UM_LONG;				]
  | [			`UM_LONG;		`UM_SIGNED;	]
  | [`UM_INT;		`UM_LONG;		`UM_SIGNED;	] -> ST_SLong
  | [			`UM_LONG;		`UM_UNSIGNED;	]
  | [`UM_INT;		`UM_LONG;		`UM_UNSIGNED;	] -> ST_ULong

  | [			`UM_LONG; `UM_LONG;			]
  | [`UM_INT;		`UM_LONG; `UM_LONG;			]
  | [			`UM_LONG; `UM_LONG;	`UM_SIGNED;	]
  | [`UM_INT;		`UM_LONG; `UM_LONG;	`UM_SIGNED;	] -> ST_SLLong
  | [			`UM_LONG; `UM_LONG;	`UM_UNSIGNED;	]
  | [`UM_INT;		`UM_LONG; `UM_LONG;	`UM_UNSIGNED;	] -> ST_ULLong

  | mods ->
      let mods = Sexplib.Sexp.to_string_hum (sexp_of_modifiers mods) in
      failwith ("Malformed type: " ^ mods)


(* TODO: check whether ocv is ever something other than [] *)
let set_cv cv = function
  | TS_name (ocv, name, typenameUsed) -> TS_name (ocv @ cv, name, typenameUsed)
  | TS_simple (ocv, tid) -> TS_simple (ocv @ cv, tid)
  | TS_elaborated (ocv, tint, name) -> TS_elaborated (ocv @ cv, tint, name)
  | TS_classSpec (ocv, tint, name, base, mems) -> TS_classSpec (ocv @ cv, tint, name, base, mems)
  | TS_enumSpec (ocv, name, enums) -> TS_enumSpec (ocv @ cv, name, enums)
  | TS_typeof (ocv, typeof) -> TS_typeof (ocv @ cv, typeof)
