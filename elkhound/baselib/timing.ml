let trace_progress = true

let start = Unix.gettimeofday ()


let progress desc f x =
  let result = f x in
  let finish = Unix.gettimeofday () in

  if trace_progress then (
    print_string "%%% ";
    Printf.printf "[%fs] %s\n" (finish -. start) desc;
    flush stdout;
  );

  result


let time desc f x =
  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  if trace_progress then (
    print_string "%%% ";
    Printf.printf "%s took %fs\n" desc (finish -. start);
    flush stdout;
  );

  result
