let time desc f x =
  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  print_string "%%% ";
  Printf.printf "%s took %f seconds\n" desc (finish -. start);
  flush stdout;

  result
