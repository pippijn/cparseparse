open Ast


let rec bottom d prev =
  match d with
  | D_bitfield _
  | D_name _ ->
      prev

  | D_attribute (base, _)
  | D_grouping (base) ->
      bottom base prev

  | D_pointer (_, base)
  | D_reference (_, base)
  | D_ptrToMember (_, _, base)
  | D_func (base, _, _, _)
  | D_array (base, _) ->
      bottom base d


(* true if this declarator is "obviously" declaring a function type,
 * i.e. the innermost non-D_name, non-D_grouping constructor is
 * D_func *)
let bottom_is_dfunc d =
  match bottom d d with
  | D_func _ -> true
  | _ -> false


(* dig down and find the name being declared (or None) *)
let rec id_of_declarator = function
  (* the ability to simply return 'name' here is why bitfields contain
   * a pq_name instead of just a string *)
  | D_bitfield (n, _)
  | D_name n -> n

  | D_attribute (base, _)
  | D_grouping (base)
  | D_pointer (_, base)
  | D_reference (_, base)
  | D_ptrToMember (_, _, base)
  | D_func (base, _, _, _)
  | D_array (base, _) ->
      id_of_declarator base


let name_has_qualifiers = function
  | PQ_ambig (PQ_qualifier _, PQ_qualifier _)
  | PQ_qualifier _ -> true

  | PQ_ambig (_, PQ_qualifier _)
  | PQ_ambig (PQ_qualifier _, _) -> failwith "ambiguous name"

  | PQ_template _
  | PQ_operator _
  | PQ_ambig _
  | PQ_name _ -> false


let spec_of_type_id = function
  | T_ambig _ -> failwith "ambiguous type"
  | T_type (spec, decl) -> spec


let decl_of_type_id = function
  | T_ambig _ -> failwith "ambiguous type"
  | T_type (spec, decl) -> decl


(* disambiguation for cppstd 14.1 para 3 *)
let can_be_type_param = function
  | TS_elaborated (cv, TI_Class, name) -> true

  (* cppstd 14.1 para 2 "typename followed by a qualified-name denotes the
   * type in a non-type parameter-declaration."  If the specifier is
   * qualified, then "typename" is not used as a type param. *)
  | TS_name ([], name, typenameUsed) ->
      typenameUsed && not (name_has_qualifiers name)

  | _ -> false


let rec any_have_default_args = function
  | None -> false
  | Some tp ->
      match tp with
      | TP_nontype (T_type (_, DC_decl (_, Some _)), _)
      | TP_template (_, _, Some _, _)
      | TP_type (_, Some _, _) ->
          true

      | TP_nontype (T_type (_, DC_decl (_, None)), next)
      | TP_template (_, _, None, next)
      | TP_type (_, None, next) ->
          any_have_default_args next

      | TP_nontype (T_type (_, DC_ambig _), _) ->
          failwith "ambiguous declaration in non-type template parameter"
      | TP_ambig _ ->
          failwith "ambiguous template parameter"
      | TP_nontype (T_ambig _, _) ->
          failwith "ambiguous non-type template parameter"


let rec ideclr_is_func = function
  | D_grouping (base) -> ideclr_is_func base
  | D_func _ -> true
  | _ -> false


let rec declr_is_func = function
  | DC_decl (decl, init) -> ideclr_is_func decl
  | DC_ambig _ -> failwith "ambiguous declarator"
