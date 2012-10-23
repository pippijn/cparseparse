open Ast


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
