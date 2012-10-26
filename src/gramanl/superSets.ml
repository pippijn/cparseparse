open GrammarType


let compute_supersets indexed_nonterms nonterms =
  (* first, resolve all subset nonterminals *)
  NtArray.iter (fun super ->
    super.subsets <-
      List.map (fun sub ->
        (* we validated the existence of all subsets, already *)
        (LocStringMap.find sub nonterms).nbase.index_id
      ) super.subset_names
  ) indexed_nonterms;

  NtArray.iter (fun super ->
    List.iter (fun sub ->
      let sub = NtArray.get indexed_nonterms sub in
      match sub.superset with
      | Some _ ->
          (* for now, only handle 'super' as a partial function *)
          failwith "nonterminal has more than one superset";
      | None ->
          sub.superset <- Some super.nbase.index_id
    ) super.subsets
  ) indexed_nonterms
