open GrammarType


let compute_supersets indexed_nonterms nonterms =
  (* first, resolve all subset nonterminals *)
  NtArray.iter (fun super ->
    super.subsets <-
      List.map (fun sub ->
        (* we validated the existence of all subsets, already *)
        (StringMap.find sub nonterms).nt_index
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
          sub.superset <- Some super.nt_index
    ) super.subsets
  ) indexed_nonterms
