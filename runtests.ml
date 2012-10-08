(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                     Simple testing infrastructure                     | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

#use "topfind"
#require "unix"

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                Execute command and build list oflines                 | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let execute cmd =
  let ic = Unix.open_process_in cmd and lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file -> ignore (Unix.close_process_in ic); List.rev !lst

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Execute tests                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let () =
  let contents = Sys.readdir "testsuite" in
  Array.sort String.compare contents;
  let run full_dir =
    let file nm = Printf.sprintf "testsuite/%s/%s" full_dir nm in
    let cmd = Printf.sprintf "./main.native %s 2>&1 >/dev/null" (file "test.cc") in
    let contents stream =
      let output_lines = ref [] in
      try while true do output_lines := input_line stream :: !output_lines done; assert false
      with End_of_file -> List.rev !output_lines
    in
    let stream = Unix.open_process_in cmd in
    let produced = contents stream in
    ignore(Unix.close_process_in stream);
    let stream = open_in (file "test.ref") in
    let expected = contents stream in
    close_in stream;
    if produced = expected
    then Printf.printf "%s => OK\n" full_dir
    else begin
      Printf.printf "%s => FAIL\n" full_dir;
      List.iter (fun line -> Printf.printf "+\t%s\n" line) produced;
      List.iter (fun line -> Printf.printf "-\t%s\n" line) expected;
    end
  in
  Array.iter run contents
