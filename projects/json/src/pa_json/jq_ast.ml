open Camlp4.PreCast (* for Ast refs in generated code *)

module AQ = Syntax.AntiquotSyntax


type aq =
  | Aq_none
  | Aq_alist
  | Aq_list
  | Aq_str
  | Aq_flo
  | Aq_int
  | Aq_bool


type t =
  | Jq_null
  | Jq_bool   of bool
  | Jq_int    of int
  | Jq_float  of float
  | Jq_string of string
  | Jq_array  of t list * string
  | Jq_object of (string * t) list
  | Jq_Ant    of Loc.t * aq * string


let string_of_aq = function
  | Aq_none -> ""
  | Aq_alist -> "alist"
  | Aq_list -> "list"
  | Aq_str -> "str"
  | Aq_flo -> "flo"
  | Aq_int -> "int"
  | Aq_bool -> "bool"


module MetaExpr = struct
  let rec meta_t _loc = function
    | Jq_null ->
        <:expr<`Null>>

    | Jq_bool b ->
        <:expr<`Bool $`bool:b$>>

    | Jq_int i ->
        <:expr<`Int $`int:i$>>

    | Jq_float f ->
        <:expr<`Float $`flo:f$>>

    | Jq_string s ->
        <:expr<`String $`str:s$>>

    | Jq_array (l, r) ->
        <:expr<`List $meta_array _loc r l$>>

    | Jq_object o ->
        <:expr<`Assoc $meta_object _loc o$>>

    | Jq_Ant (_loc, kind, code) ->
        let p = AQ.parse_expr _loc code in
        match kind with
        | Aq_none  -> p
        | Aq_alist -> <:expr<`Assoc  $p$>>
        | Aq_list  -> <:expr<`List   $p$>>
        | Aq_str   -> <:expr<`String $p$>>
        | Aq_flo   -> <:expr<`Float  $p$>>
        | Aq_int   -> <:expr<`Int    $p$>>
        | Aq_bool  -> <:expr<`Bool   $p$>>


  and meta_array _loc rest = function
    | [] ->
        begin match rest with
        | "" ->
            <:expr<[]>>
        | "_" ->
            failwith ".. requires list name"
        | rest ->
            <:expr<$lid:rest$>>
        end
    | hd :: tl ->
        <:expr<[$meta_t _loc hd$ :: $meta_array _loc rest tl$]>>


  and meta_object _loc = function
    | [] ->
        <:expr<[]>>
    | (key, value) :: tl ->
        <:expr<[($`str:key$, $meta_t _loc value$) :: $meta_object _loc tl$]>>
end


module MetaPatt = struct
  let rec meta_t _loc = function
    | Jq_null ->
        <:patt<`Null>>

    | Jq_bool b ->
        <:patt<`Bool $`bool:b$>>

    | Jq_int i ->
        <:patt<`Int $`int:i$>>

    | Jq_float f ->
        <:patt<`Float $`flo:f$>>

    | Jq_string s ->
        <:patt<`String $`str:s$>>

    | Jq_array (l, r) ->
        <:patt<`List $meta_array _loc r l$>>

    | Jq_object o ->
        <:patt<`Assoc $meta_object _loc o$>>

    | Jq_Ant (_loc, kind, code) ->
        let p = AQ.parse_patt _loc code in
        match kind with
        | Aq_none  -> p
        | Aq_alist -> <:patt<`Assoc $p$>>
        | Aq_list  -> <:patt<`List $p$>>
        | Aq_str   -> <:patt<`String $p$>>
        | Aq_flo   -> <:patt<`Float $p$>>
        | Aq_int   -> <:patt<`Int $p$>>
        | Aq_bool  -> <:patt<`Bool $p$>>


  and meta_array _loc rest = function
    | [] ->
        begin match rest with
        | "" ->
            <:patt<[]>>
        | "_" ->
            <:patt<_>>
        | rest ->
            <:patt<$lid:rest$>>
        end
    | hd :: tl ->
        <:patt<[$meta_t _loc hd$ :: $meta_array _loc rest tl$]>>


  and meta_object _loc = function
    | [] ->
        <:patt<[]>>
    | (key, value) :: tl ->
        <:patt<[($`str:key$, $meta_t _loc value$) :: $meta_object _loc tl$]>>
end
