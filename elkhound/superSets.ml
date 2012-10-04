open GrammarType


let compute_supersets indexed_nonterms nonterms =
  Array.iter (fun super ->
    List.iter (fun sub ->
      (* we validated the existence of all subsets, already *)
      let sub = Stringmap.find sub nonterms in
      match sub.superset with
      | Some _ ->
          (* for now, only handle 'super' as a partial function *)
          failwith "nonterminal has more than one superset";
      | None ->
          sub.superset <- Some super
    ) super.subsets
  ) indexed_nonterms
