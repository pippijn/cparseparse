let _trace_progress = ref false


let specs = ref Arg.([
  "-timing",		Set _trace_progress,		" output timing details";
])
let actions = ref []


let register ?action spec =
  specs := spec @ !specs;
  match action with
  | None -> ()
  | Some action ->
      actions := action :: !actions


let run f =
  let inputs = ref [] in

  Arg.(parse (align !specs)
    (fun input -> inputs := input :: !inputs)
    ("Usage: " ^ Sys.argv.(0) ^ " [option...] <input...>")
  );

  let inputs = List.rev !inputs in

  List.iter (fun f -> f inputs) !actions;

  f inputs


let _trace_progress () = !_trace_progress
