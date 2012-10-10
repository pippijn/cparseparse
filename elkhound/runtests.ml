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

let base_dir = "testsuite/ccparse"

let slurp stream =
  let output_lines = ref [] in
  try while true do output_lines := input_line stream :: !output_lines done; assert false
  with End_of_file -> List.rev !output_lines

let isdigit = function
  | '0' .. '9' -> true
  | _ -> false

let run (pass, fail) full_dir =
  if isdigit full_dir.[0] then
    let file nm = Printf.sprintf "%s/%s/%s" base_dir full_dir nm in

    (* Retrieve command line options from file *)
    let opts =
      let stream = open_in (file "test.cc") in
      let line = input_line stream in
      close_in stream;

      if line.[0] = '/' && line.[1] = '/' && line.[2] = '+' then
        String.sub line 3 (String.length line - 3)
      else
        ""
    in

    let start = Unix.gettimeofday () in

    (* Parse the file *)
    let cmd = Printf.sprintf "_build/ccparse/ccparse.native -trivial %s %s 2>&1" opts (file "test.cc") in
    let stream = Unix.open_process_in cmd in
    let produced = slurp stream in
    ignore (Unix.close_process_in stream);

    let finish = Unix.gettimeofday () in

    (* Read the expected output *)
    let stream = open_in (file "test.ref") in
    let expected = slurp stream in
    close_in stream;

    (* Compare produced and expected outputs *)
    let result =
      let time = finish -. start in
      if produced = expected then (
        Printf.printf "[PASS] %s (%fs)\n" full_dir time;
        (pass + 1, fail)
      ) else (
        Printf.printf "[FAIL] %s (%fs)\n" full_dir time;
        List.iter (fun line -> Printf.printf "+\t%s\n" line) produced;
        List.iter (fun line -> Printf.printf "-\t%s\n" line) expected;
        (pass, fail + 1)
      )
    in

    flush stdout;
    result
  else
    (pass, fail)


let () =
  let contents = Sys.readdir base_dir in
  (* Sort the tests lexicograpically *)
  Array.sort String.compare contents;
  (* Run the tests *)
  let pass, fail = Array.fold_left run (0, 0) contents in
  print_endline "-----------------------------";
  Printf.printf "  Summary: %d PASS, %d FAIL\n" pass fail;
  print_endline "-----------------------------"
