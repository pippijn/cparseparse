open Sexplib.Conv
open Gramast
module Stringmap = Stringmap.M
module Stringset = Stringset.M

module ProdDeclMap = Map.Make(struct

  type t = proddecl

  let compare_rhs al bl =
    let all = List.length al in
    let bll = List.length bl in
    if all <> bll then
      compare all bll
    else
      List.fold_left2 (fun direction a b ->
        if direction = 0 then
          match a, b with
          | RH_name   (_, aname), RH_name   (_, bname) -> compare aname bname
          | RH_string (_, astr ), RH_string (_, bstr ) -> compare astr  bstr
          | RH_prec   (atok    ), RH_prec   (btok    ) -> compare atok  btok
          | RH_forbid (atok    ), RH_forbid (btok    ) -> failwith "don't know how to compare forbids"

          | RH_name _, RH_string _ -> -1
          | RH_name _, RH_prec _ -> -1
          | RH_name _, RH_forbid _ -> -1
          | RH_string _, RH_prec _ -> -1
          | RH_string _, RH_forbid _ -> -1
          | RH_prec _, RH_forbid _ -> -1

          | RH_forbid _, RH_prec _ -> 1
          | RH_forbid _, RH_string _ -> 1
          | RH_forbid _, RH_name _ -> 1
          | RH_prec _, RH_string _ -> 1
          | RH_prec _, RH_name _ -> 1
          | RH_string _, RH_name _ -> 1
        else
          direction
      ) 0 al bl

  let compare (ProdDecl (_, arhs, _)) (ProdDecl (_, brhs, _)) =
    compare_rhs arhs brhs

end)



let accumulators =
  List.fold_left (fun set elt -> Stringset.add elt set) Stringset.empty [
    "shift_reduce_conflicts";
    "reduce_reduce_conflicts";
    "unreachable_nonterminals";
    "unreachable_terminals";
  ]


type nonterm = {
  funcs : specfunc Stringmap.t;
  prods : proddecl ProdDeclMap.t;
  subsets : string list;
}

type topforms = {
  verbatims : topform list;
  options : topform Stringmap.t;

  (* terminals *)
  decls : termdecl list;
  types : termtype list;
  precs : precspec list;

  nonterms : topform Stringmap.t;
  nterms : nonterm Stringmap.t;
}

let empty_topforms = {
  verbatims = [];
  options = Stringmap.empty;

  decls = [];
  types = [];
  precs = [];

  nonterms = Stringmap.empty;
  nterms = Stringmap.empty;
}


let nonterms nterms =
  List.map (fun (name, nterm) ->
    TF_nonterm (
      name,
      snd (List.split (Stringmap.bindings nterm.funcs)),
      snd (List.split (ProdDeclMap.bindings nterm.prods)),
      nterm.subsets
    )
  ) (Stringmap.bindings nterms)


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


let merge_funcs2 nfuncs ofuncs =
  (* Replace all old functions with new functions. *)
  List.fold_left (fun funcs (SpecFunc (nname, _, _) as nfunc) ->
    Stringmap.add nname nfunc funcs
  ) ofuncs nfuncs


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


let merge_prods2 nprods oprods =
  (* Iterate over the new productions, yield the merged list. *)
  List.fold_left (fun prods (ProdDecl (nkind, nrhs, ncode) as nprod) ->
    try
      (* If the old production RHS matches the new one *)
      let existing = ProdDeclMap.find nprod prods in
      (* We check the instruction for the new production. *)
      match nkind with
      | PDK_NEW ->
          failwith "production has the same RHS as an existing production; if intent is to replace, use the 'replace' keyword"
      | PDK_DELETE ->
          (* Drop both old and new production. *)
          prods
      | PDK_REPLACE ->
          (* Add new production, drop old one. *)
          ProdDeclMap.add existing nprod prods
    with Not_found ->
      (* The new production does not match any in the old list. *)
      match nkind with
      | PDK_NEW ->
          (* So we can add it. *)
          ProdDeclMap.add nprod nprod prods
      | PDK_DELETE ->
          failwith "production marked with 'delete' does not match any in the base specification"
      | PDK_REPLACE ->
          failwith "production marked with 'replace' does not match any in the base specification"
  ) oprods nprods


let merge_prods nprods oprods =
  let merged =
    (* Iterate over the new productions, yield the merged list. *)
    List.fold_left (fun oprods (ProdDecl (nkind, nrhs, ncode) as nprod) ->
      (* found: true if the old list contained this production
       * prods: old list; if found is true, one production was deleted or replaced, otherwise the list is unchanged *)
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


let merge grammars =
  let use_map = true in

  let topforms =
    List.fold_left (fun topforms (file, grammar) ->
      List.fold_left (fun topforms topform ->
        match topform with
        | TF_verbatim _ ->
            { topforms with verbatims = topform :: topforms.verbatims }

        | TF_option (name, value) when Stringset.mem name accumulators ->
            let option =
              try
                match Stringmap.find name topforms.options with
                | TF_option (_, ovalue) ->
                    TF_option (name, value + ovalue)
                | _ ->
                    failwith "match"
              with Not_found ->
                topform
            in

            { topforms with
              options = Stringmap.add name option topforms.options
            }

        | TF_option (name, _) ->
            { topforms with
              options = Stringmap.add name topform topforms.options
            }

        | TF_terminals (decls, types, precs) ->
            { topforms with
              decls = decls @ topforms.decls;
              types = types @ topforms.types;
              precs = precs @ topforms.precs;
            }

        | TF_nonterm (name, nfuncs, nprods, nsubsets) ->
            if not use_map then
              let topform =
                try
                  (* Find an existing non-terminal. *)
                  match Stringmap.find name topforms.nonterms with

                  | TF_nonterm (_, ofuncs, oprods, osubsets) ->
                      let funcs = merge_funcs nfuncs ofuncs in
                      let prods = merge_prods nprods oprods in
                      (* TODO: how to handle subset merges? (what are subsets?) *)
                      let subsets = nsubsets @ osubsets in

                      TF_nonterm (name, funcs, prods, subsets)

                  | _ ->
                      failwith "match"

                with Not_found ->
                  topform
              in
              { topforms with
                nonterms = Stringmap.add name topform topforms.nonterms;
              }

            else

              let nterm =
                try
                  let existing = Stringmap.find name topforms.nterms in
                  {
                    funcs = merge_funcs2 nfuncs existing.funcs;
                    prods = merge_prods2 nprods existing.prods;
                    (* TODO: how to handle subset merges? (what are subsets?) *)
                    subsets = nsubsets @ existing.subsets;
                  }
                with Not_found ->
                  {
                    funcs = merge_funcs2 nfuncs Stringmap.empty;
                    prods = merge_prods2 nprods ProdDeclMap.empty;
                    subsets = nsubsets;
                  }
              in
              { topforms with
                nterms = Stringmap.add name nterm topforms.nterms;
              }

      ) topforms grammar
    ) empty_topforms grammars
  in

  let merged =
    snd (List.split (Stringmap.bindings topforms.options))
    @ topforms.verbatims
    @ [TF_terminals (topforms.decls, topforms.types, topforms.precs)]
  in

  merged @ (
    if not use_map then
      snd (List.split (Stringmap.bindings topforms.nonterms))
    else
      nonterms topforms.nterms
  )
