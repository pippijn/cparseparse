let _trace_progress = ref false
let _alloc_stats = ref false


let specs = let open Arg in ref ([
  "-timing",		Set _trace_progress,		" output timing details";
  "-alloc-stats",	Set _alloc_stats,		" print memory allocation statistics (implies -timing)";
])
let actions = ref []


let _trace_progress () = !_trace_progress
let _alloc_stats () = !_alloc_stats


let register ?action spec =
  specs := spec @ !specs;
  match action with
  | None -> ()
  | Some action ->
      actions := action :: !actions


let run f =
  let inputs = ref [] in

  let specs =
    List.map (fun (arg, kind, desc) ->
      let default =
        match kind with
        | Arg.Set ref -> string_of_bool !ref
        | Arg.Set_int ref -> string_of_int !ref
        | Arg.Set_string ref -> !ref
        | _ -> failwith "unsupported argument kind"
      in

      (arg, kind, desc ^ " (default: " ^ default ^ ")")
    ) !specs
  in

  Arg.(parse (align specs)
    (fun input -> inputs := input :: !inputs)
    ("Usage: " ^ BatStd.exe ^ " [option...] <input...>")
  );

  let inputs = List.rev !inputs in

  List.iter (fun f -> f inputs) !actions;

  f inputs
