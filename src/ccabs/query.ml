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


let tp_has_default_arg = function
  | TP_nontype (T_type (_, DC_decl (_, Some _)))
  | TP_template (_, _, Some _)
  | TP_type (_, Some _) -> true

  | TP_nontype (T_type (_, DC_decl (_, None)))
  | TP_template (_, _, None)
  | TP_type (_, None) -> false

  | TP_nontype (T_type (_, DC_ambig _)) -> failwith "ambiguous declaration in non-type template parameter"
  | TP_ambig _ -> failwith "ambiguous template parameter"
  | TP_nontype (T_ambig _) -> failwith "ambiguous non-type template parameter"


let any_have_default_args tps =
  try
    ignore (List.find tp_has_default_arg tps);
    true
  with Not_found ->
    false


let rec disambig_nontype_ta = let open BatOption.Monad in function
  | E_ambig (l, r) ->
      (* We are asking whether an ambiguous expression has an
       * unparenthesized greater-than operator (UGTO), because the
       * parser wants to reject such things.  But this expression is
       * ambiguous!  So, if some of the alternatives contain UGTO
       * but others do not, simply remove the UGTO alternatives and
       * then return the ones that are left.  If they *all* have
       * UGTO, return None. *)
      let expr =
        match disambig_nontype_ta l, disambig_nontype_ta r with
        (* all have UGTO *)
        | None, None -> None

        (* one alternative left *)
        | None, Some e
        | Some e, None -> Some l

        (* both left *)
        | Some l, Some r -> Some (E_ambig (l, r))
      in

      expr

  (* all this just to find one little guy.. *)
  | E_binary (_, BIN_GREATER, _) ->
      None

  (* recursively dig down into any subexpressions which syntactically
   * aren't enclosed in parentheses or brackets *)
  | E_assign (left, op, right) ->
      perform
        left <-- disambig_nontype_ta left;
        right <-- disambig_nontype_ta right;
        return (E_assign (left, op, right))

  | E_binary (left, op, right) ->
      perform
        left <-- disambig_nontype_ta left;
        right <-- disambig_nontype_ta right;
        return (E_binary (left, op, right))

  | E_cond (cond, thenExpr, elseExpr) ->
      perform
        cond <-- disambig_nontype_ta cond;
        thenExpr <-- disambig_nontype_ta thenExpr;
        elseExpr <-- disambig_nontype_ta elseExpr;
        return (E_cond (cond, thenExpr, elseExpr))

  | E_funCall (expr, args) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_funCall (expr, args))

  | E_fieldAcc (expr, member) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_fieldAcc (expr, member))

  | E_unary (op, expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_unary (op, expr))

  | E_effect (op, expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_effect (op, expr))

  | E_addrOf (expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_addrOf (expr))

  | E_deref (expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_deref (expr))

  | E_cast (ctype, expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_cast (ctype, expr))

  | E_delete (global, array, expr) ->
      perform
        expr <-- disambig_nontype_ta expr;
        return (E_delete (global, array, expr))

  (* everything else, esp. E_grouping, is false *)
  | E_grouping _
  | E_keywordCast _
  | E_boolLit _
  | E_intLit _
  | E_floatLit _
  | E_stringLit _
  | E_charLit _
  | E_this
  | E_variable _
  | E_constructor _
  | E_sizeofExpr _
  | E_sizeofType _
  | E_new _
  | E_throw _
  | E_arrow _
  | E_typeidExpr _
  | E_typeidType _ as e ->
      return e


let rec ideclr_is_func = function
  | D_grouping (base) -> ideclr_is_func base
  | D_func _ -> true
  | _ -> false


let rec declr_is_func = function
  | DC_decl (decl, init) -> ideclr_is_func decl
  | DC_ambig _ -> failwith "ambiguous declarator"
