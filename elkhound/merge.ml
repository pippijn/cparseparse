open Gramast
module Stringmap = Stringmap.M
module Stringset = Stringset.M


let accumulators =
  List.fold_left (fun set elt -> Stringset.add elt set) Stringset.empty [
    "shift_reduce_conflicts";
    "reduce_reduce_conflicts";
    "unreachable_nonterminals";
    "unreachable_terminals";
  ]


type topforms = {
  verbatims : topform list;
  options : topform list;

  (* terminals *)
  decls : termdecl list;
  types : termtype list;
  precs : precspec list;

  nonterms : topform Stringmap.t;
}

let empty_topforms = {
  verbatims = [];
  options = [];

  decls = [];
  types = [];
  precs = [];

  nonterms = Stringmap.empty;
}


let merge_funcs nfuncs ofuncs =
  (* Iterate over the old functions *)
  List.fold_left (fun funcs (SpecFunc (oname, oformals, ocode) as ofunc) ->
    let func =
      try
        (* If the new function list contains a function with the same name,
         * replace the old function with it. *)
        List.find (function SpecFunc (nname, _, _) -> nname = oname) nfuncs
      with Not_found ->
        (* Otherwise, keep the old function. *)
        ofunc
    in

    func :: funcs
  ) [] ofuncs


let rhs_equal al bl =
  try
    List.fold_left2 (fun equal a b ->
      equal &&
        match a, b with
        | RH_name   (_, aname), RH_name   (_, bname) -> aname = bname
        | RH_string (_, astr ), RH_string (_, bstr ) -> astr  = bstr
        | RH_prec   (atok    ), RH_prec   (btok    ) -> atok  = btok
        | _ -> false
    ) true al bl
  with Invalid_argument _ ->
    false


let merge_prods nprods oprods =
  let merged =
    (* Iterate over the new productions, yield the merged list. *)
    List.fold_left (fun oprods (ProdDecl (nkind, nrhs, ncode) as nprod) ->
      (* found: true if the old list contained this production
       * prods: old list; if found is true, one production was deleted or replaced,
       *        otherwise the list is unchanged *)
      let found, prods =
        List.fold_left (fun (found, prods) (ProdDecl (okind, orhs, ocode) as oprod) ->
          (* If the old production RHS matches the new one *)
          if rhs_equal nrhs orhs then

            (* We check the instruction for the new production. *)
            match nkind with
            | PDK_NEW ->
                failwith "production has the same RHS as an existing production; if intent is to replace, use the 'replace' keyword"
            | PDK_DELETE ->
                (* Drop both old and new production. *)
                (true, prods)
            | PDK_REPLACE ->
                (* Add new production, drop old one. *)
                (true, nprod :: prods)

          else

            (* Otherwise, we keep the old production. *)
            (found, oprod :: prods)

        ) (false, []) oprods
      in

      if found then
        (* The new production replaced or deleted an old one. *)
        prods
      else
        (* The new production does not match any in the old list. *)
        match nkind with
        | PDK_NEW ->
            (* So we can add it. *)
            nprod :: prods
        | PDK_DELETE ->
            failwith "production marked with 'delete' does not match any in the base specification"
        | PDK_REPLACE ->
            failwith "production marked with 'replace' does not match any in the base specification"

    ) oprods nprods
  in

  assert (List.length merged >= List.length oprods);

  merged


let list_merge a b =
  if List.length a < List.length b then
    a @ b
  else
    b @ a


let merge grammars =
  let topforms =
    List.fold_left (fun topforms (file, grammar) ->
      List.fold_left (fun topforms topform ->
        match topform with
        | TF_verbatim _ ->
            { topforms with verbatims = topform :: topforms.verbatims }

        | TF_option (name, value) when Stringset.mem name accumulators ->
            (* Sum up values for some options. *)
            let found, options =
              List.fold_left (fun (found, options) -> function
                | TF_option (oname, ovalue) when oname = name ->
                    true, TF_option (name, value + ovalue) :: options
                | TF_option _ as option ->
                    found, option :: options
                | _ ->
                    failwith "match"
              ) (false, []) topforms.options
            in

            { topforms with
              options = (
                if found then
                  options
                else
                  topform :: topforms.options
              )
            }

        | TF_option (name, _) ->
            (* Overwrite the value for others. *)
            { topforms with
              options = topform :: topforms.options
            }

        | TF_terminals (decls, types, precs) ->
            { topforms with
              decls = list_merge decls topforms.decls;
              types = list_merge types topforms.types;
              precs = list_merge precs topforms.precs;
            }

        | TF_nonterm (name, nfuncs, nprods, nsubsets) ->
            let topform =
              try
                (* Find an existing non-terminal. *)
                match Stringmap.find name topforms.nonterms with

                | TF_nonterm (_, ofuncs, oprods, osubsets) ->
                    let funcs = merge_funcs nfuncs ofuncs in
                    let prods = merge_prods nprods oprods in
                    (* TODO: how to handle subset merges? (what are subsets?) *)
                    let subsets = list_merge nsubsets osubsets in

                    TF_nonterm (name, funcs, prods, subsets)

                | _ ->
                    failwith "match"

              with Not_found ->
                topform
            in
            { topforms with
              nonterms = Stringmap.add name topform topforms.nonterms;
            }

      ) topforms grammar
    ) empty_topforms grammars
  in

  []
  @ topforms.options
  @ topforms.verbatims
  @ [TF_terminals (topforms.decls, topforms.types, topforms.precs)]
  @ snd (List.split (Stringmap.bindings topforms.nonterms))
