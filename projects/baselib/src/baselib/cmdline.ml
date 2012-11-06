let section_separator section = ("\n" ^ section ^ ":", Arg.Unit ignore, "")

let specs = ref [section_separator "help"]
let actions = ref []


let register ?action section spec =
  specs := section_separator section :: spec @ !specs;
  match action with
  | None -> ()
  | Some action ->
      actions := action :: !actions


let run f =
  Printexc.record_backtrace true;
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
