let time f x =
  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  Printf.printf "took %f seconds\n" (finish -. start);

  result
