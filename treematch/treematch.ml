(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Some good runtime defaults                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let run f =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };
  Printexc.record_backtrace true;
  Printexc.print f Options.inputs

open Options

let () = run (fun _ -> ())
