open GrammarType


let compute_supersets indexed_nonterms nonterms =
  (* first, resolve all subset nonterminals *)
  let indexed_nonterms =
    NtArray.map (fun super ->
      { super with subsets =
        List.map (fun sub ->
          (* we validated the existence of all subsets, already *)
          (LocStringMap.find sub nonterms).nbase.index_id
        ) super.subset_names
      }
    ) indexed_nonterms
  in

  let superset = NtArray.make (NtArray.length indexed_nonterms) None in

  let superset_of nt_index =
    NtArray.get superset nt_index
  in

  let set_superset_of nt_index set =
    NtArray.set superset nt_index set
  in

  NtArray.iter (fun super ->
    List.iter (fun sub ->
      match superset_of sub with
      | Some _ ->
          (* for now, only handle 'super' as a partial function *)
          failwith "nonterminal has more than one superset";
      | None ->
          set_superset_of sub (Some super.nbase.index_id)
    ) super.subsets
  ) indexed_nonterms;

  (* update nonterms *)
  NtArray.mapi (fun nt_index nonterm ->
    { nonterm with superset = NtArray.get superset nt_index }
  ) indexed_nonterms
