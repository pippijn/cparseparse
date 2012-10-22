open Ast


let rec bottom d prev =
  match d with
  | D_bitfield _
  | D_name _ ->
      prev

  | D_grouping (base) ->
      bottom base prev

  | D_pointer (_, base)
  | D_reference (_, base)
  | D_ptrToMember (_, _, base)
  | D_func (base, _, _, _)
  | D_array (base, _) ->
      bottom base d


let bottom_is_dfunc d =
  match bottom d d with
  | D_func _ -> true
  | _ -> false


let rec id_of_declarator = function
  (* the ability to simply return 'name' here is why bitfields contain
   * a pq_name instead of just a string *)
  | D_bitfield (n, _)
  | D_name n -> n

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
