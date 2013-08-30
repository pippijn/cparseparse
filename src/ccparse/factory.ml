open CorePervasives
open Ccabs
open Ast

let cancel = UserActions.cancel


(* make a D_func but not if it attempts to be the return value of
 * another function (this helps resolve an ambiguity in the presence
 * of implicit int.. is it needed even without implicit int?) *)
let make_d_func base params cv exnspec =
  match base with
  | D_func _ ->
      cancel "function returning a function"
  | _ ->
      D_func (base, params, cv, exnspec)


let make_d_array base size =
  match base with
  | D_func _ ->
      cancel "function returning an array"
  | _ ->
      D_array (base, size)


(* when this is the last element in a parameter list, the function
 * is a vararg function *)
let make_ellipsis_type_id () =
  (* dsw: NOTE: I use SL_GENERATED here for the Declarator and the
   * D_name because they are just abstract things anyway and the end
   * of the compound statement hardly seems like the "right" end
   * point for them *)
  T_type (
    TS_simple ([], ST_Ellipsis),
    DC_decl (
      D_name (None (* name *)),
      None (* initialiser *)
    )
  )


let declarator = function
  | DC_decl (decl, _) -> decl
  | DC_ambig _ as declr ->
      Printf.printf "ambiguous decls: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_declarator declr));
      failwith "ambiguous decls"


let rec map_ambig_decls f result d =
  match d with
  | DC_decl _ ->
      (f d) :: result
  | DC_ambig (l, r) ->
      let result = map_ambig_decls f result l in
      let result = map_ambig_decls f result r in
      result

let map_ambig_decls f declr =
  map_ambig_decls f [] declr



let ends_with_identifier = function
  | TS_name _
  | TS_elaborated _ -> true
  | _ -> false


let is_global_scope_qualified = function
  | PQ_qualifier (None, _, _) -> true
  | _ -> false


(* Filter out declarations of the form "TYPENAME ::NAME", since
 * other front-ends would regard the TYPENAME as a qualifier.
 *
 * The performance could be improved by doing static disambiguation,
 * however that is a little tricky because the productions involved
 * conflict with several rules, and I need to resolve conflicts with
 * just certain pairs of rules.  So, until I implement a more
 * precise form of static disambiguation in Elkhound, I will just
 * resort to runtime filtering. *)
let keep_declaration d =
  match d.decllist with
  | hd :: _ when ends_with_identifier d.spec ->
      let rec loop id =
        match id with
        (* skip past type constructors that come *after* the name;
         * if they come before, then they separate the :: from the
         * TYPENAME, and hence :: could be the global scope qualifier *)
        | D_func (base, _, _, _)
        | D_array (base, _) ->
            loop base

        | D_name (name) ->
            not (is_global_scope_qualified (CoreOption.get name))

        | _ ->
            (* something precedes ::, so it is ok *)
            true
      in

      let keeps =
        map_ambig_decls (fun declr ->
          loop (declarator declr)
        ) hd
      in

      let keep = List.for_all identity keeps in

      if keep then (
        (* either all ambiguous alternatives say "keep" *)
      ) else (
        (* or none say "keep" *)
        assert (not (List.exists identity keeps));
        cancel "due to TYPENAME ::NAME"
      );

      keep

  | _ ->
      true



(* ambiguous:
 *   F::G() {}
 * can either be a definition of F's constructor (then G equals F), or
 * it can be a definition of a function G in the global scope, with
 * return type F
 *
 * cppstd isn't clear about this, but both gcc and icc eagerly consume
 * "::" after an identifier, so I cancel a function definition if the
 * retspec is a typedef and the name begins with "::" *)
let keep_function_definition f =
  let declr = declarator f.name_and_params in
  assert (Query.bottom_is_dfunc declr);

  let n = Query.id_of_declarator declr in
  if ends_with_identifier f.retspec && is_global_scope_qualified (CoreOption.get n) then
    cancel "rejecting TYPENAME ::NAME"
  else
    true


let merge_template_argument_list l r =
  Some (TA_ambig (CoreOption.get l, CoreOption.get r))


let merge_template_parameter_list l r =
  Some (TP_ambig (CoreOption.get l, CoreOption.get r))
