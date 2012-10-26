let _trace_progress = ref false
let _alloc_stats = ref false
let _alloc_timing = ref false

let dummy_spec = ("", Arg.Unit ignore, "")

let specs = let open Arg in ref ([
  "-timing",		Set _trace_progress,		" output timing details";
  "-alloc-stats",	Set _alloc_stats,		" print memory allocation statistics";
  "-alloc-timing",	Set _alloc_timing,		" print alloc statistics in timings (implies -timing)";
  dummy_spec;
])
let actions = ref [
  (fun inputs ->
    if !_alloc_timing then
      _trace_progress := true;
  )
]


let _trace_progress () = !_trace_progress
let _alloc_stats () = !_alloc_stats
let _alloc_timing () = !_alloc_timing


let register ?action spec =
  specs := spec @ dummy_spec :: !specs;
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
        (* pass these on *)
        | Arg.Unit _ -> None

        | Arg.Set ref -> Some (string_of_bool !ref)
        | Arg.Set_int ref -> Some (string_of_int !ref)
        | Arg.Set_string ref -> Some !ref
        | _ -> failwith "unsupported argument kind"
      in

      match default with
      | None ->
          (arg, kind, desc)
      | Some default ->
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
