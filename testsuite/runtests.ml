module Options = struct
  let test_suffixes = [".cc"; ".c"; ".ii"; ".i"; ".tm"]
  let tool_opts = StringMap.of_list [
    "cpapa", "-trivial";
  ]
  let dirs = [
    "treematch", "testsuite/treematch";
    "cpapa", "testsuite/ccparse";
    "cpapa", "testsuite/in";
  ]
end


(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Colours for terminal output                  | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let colourise = not (BatArray.mem "--no-colour" Sys.argv)
let if_colour s = if colourise then s else ""

let green = if_colour "[1;32m"
let red   = if_colour "[1;31m"
let reset = if_colour "[0m"


(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                Execute command and build list of lines                | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let execute cmd =
  let ic = Unix.open_process_in cmd and lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file -> ignore (Unix.close_process_in ic); List.rev !lst


(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Helper functions                          | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)


let (/) a b = a ^ "/" ^ b


let output_newline out =
  output_string out "\n"

let output_endline out line =
  output_string out line;
  output_newline out


let output_underline dot out str =
  ignore (BatString.fold_left (fun skip c ->
    match c with
    | '' -> true
    | 'm' when skip -> false
    | _ when skip -> true

    | _ ->
        output_char out dot;
        false
  ) false str)


let with_out file f =
  let out = open_out file in
  begin try
    f out
  with e ->
    close_out out;
    raise e
  end;
  close_out out


let write_all stream lines =
  List.iter (output_endline stream) lines


let read_all stream =
  let output_lines = ref [] in
  try
    while true do
      output_lines := input_line stream :: !output_lines
    done;
    assert false
  with End_of_file ->
    List.rev !output_lines


let string_of_process_status = let open Unix in function
  | WEXITED status -> "exited with code " ^ string_of_int status
  | WSIGNALED signum -> "caught signal " ^ string_of_int signum
  | WSTOPPED signum -> "stopped by signal " ^ string_of_int signum


(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Execute tests                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let run_test error_log tool (pass, fail, total_time) source reference =
  (* Retrieve common command line options *)
  let tool_opts =
    StringMap.find_default "" tool Options.tool_opts
  in

  (* Retrieve command line options from file *)
  let opts =
    let stream = open_in source in
    let line = input_line stream in
    close_in stream;

    match ExtString.without_prefix "//+" line with
    | Some opts -> opts
    | None -> ""
  in

  (* Parse the file *)
  let start = Unix.gettimeofday () in

  let cmd = Printf.sprintf "./%s.native %s %s %s 2>&1" tool tool_opts opts source in
  let stream = Unix.open_process_in cmd in
  let produced = read_all stream in
  let status = Unix.close_process_in stream in

  let finish = Unix.gettimeofday () in

  (* Read the expected output *)
  let expected =
    try
      let stream = open_in reference in
      let expected = read_all stream in
      close_in stream;
      expected
    with Sys_error _ ->
      []
  in

  (* Compare produced and expected outputs *)
  let time = finish -. start in
  if produced = expected then (
    Printf.printf "[%sPASS%s] %s (%fs)\n" green reset source time;

    (pass + 1, fail, total_time +. time)
  ) else (
    Printf.printf "[%sFAIL%s] %s (%fs)\n" red   reset source time;

    (* Write results to error log *)
    let print_output = function
      | [] ->
          output_string error_log " no output\n"
      | lines ->
          output_string error_log ":\n\n::\n\n";
          List.iter (fun line -> Printf.fprintf error_log "  %s\n" line) lines
    in

    output_string error_log "\n\n";
    let title = Printf.sprintf "FAIL: %s *(%s)*" source (string_of_process_status status) in
    output_string error_log title;
    output_char error_log '\n';
    output_underline '-' error_log title;
    output_string error_log "\n\nexpected";
    print_output expected;
    output_string error_log "\nproduced";
    print_output produced;

    (pass, fail + 1, total_time +. time)
  )


let run error_log tool base_dir result test =
  match ExtString.without_suffixes Options.test_suffixes test with
  | Some basename ->
      let source    = base_dir / test in
      let reference = base_dir / basename ^ ".ref" in

      let result = run_test error_log tool result source reference in
      flush stdout;
      result

  | None ->
      result


let rec run_tests error_log result (tool, base_dir) =
  let contents = Sys.readdir base_dir in
  (* Sort the tests lexicograpically *)
  Array.sort String.compare contents;
  (* Separate them into dirs/files *)
  let dirs  =
    BatArray.filter (fun file ->
      Unix.((stat (base_dir / file)).st_kind = S_DIR)
    ) contents
  in
  let files =
    BatArray.filter (fun file ->
      (* Do not process expected-output files *)
      not (ExtString.ends_with ".ref" file)
      && Unix.((stat (base_dir / file)).st_kind = S_REG)
    ) contents
  in
  (* Run the tests from this dir *)
  let result = Array.fold_left (run error_log tool base_dir) result files in
  (* Run the tests from each subdir *)
  let result =
    Array.fold_left (fun result dir ->
      run_tests error_log result (tool, base_dir / dir)
    ) result dirs
  in

  result


let main () =
  with_out "testsuite.rst" (fun error_log ->
    begin
      let open Unix in
      let { tm_min; tm_hour; tm_mday; tm_mon; tm_year; } = localtime (time ()) in

      Printf.fprintf error_log "Error log for test suite run on %04d-%02d-%02d %02d:%02d\n"
        (tm_year + 1900)
        (tm_mon + 1)
        tm_mday
        tm_hour
        tm_min;
      output_string error_log "================================================";
    end;

    let pass, fail, time =
      List.fold_left (run_tests error_log) (0, 0, 0.0) Options.dirs
    in

    output_string error_log "\nSummary";
    output_string error_log "\n-------\n";

    let msg =
      if fail = 0 then (
        Printf.fprintf error_log "all %d tests PASSed\n" pass;
        Printf.sprintf "  Summary: all %d tests %sPASS%sed [%fs]  " pass green reset time
      ) else (
        Printf.fprintf error_log "%d PASS, **%d FAIL**\n" pass fail;
        Printf.sprintf "  Summary: %s%d PASS%s, %s%d FAIL%s [%fs]  "
          green pass reset
          red   fail reset
          time
      );
    in

    print_newline (); output_underline '-' stdout msg; print_newline ();
    print_string msg;
    print_newline (); output_underline '-' stdout msg; print_newline ();
  )


let () =
  Printexc.record_backtrace true;
  Printexc.print main ()
