let output_size out size =
  let size = float_of_int size /. 1024.0 in
  Printf.fprintf out "%.1f" size


let output_memsize out size =
  if abs size > 1024 * 1024 * 1024 then
    Printf.fprintf out "%aG" output_size (size / 1024 / 1024)
  else if abs size > 1024 * 1024 then
    Printf.fprintf out "%aM" output_size (size / 1024)
  else if abs size > 1024 then
    Printf.fprintf out "%aK" output_size size
  else
    Printf.fprintf out "%dB" size


let output_alloc out = function
  | Some start_stat, Some finish_stat ->
      Printf.fprintf out " (allocated %a)" output_memsize
        Gc.((finish_stat.live_words - start_stat.live_words) * Sys.word_size)
  | _ ->
      ()


let start = Unix.gettimeofday ()


let alloc_stats () =
  if Cmdline._alloc_stats () then (
    Gc.compact ();
    Some (Gc.stat ())
  ) else (
    None
  )


let progress desc f x =
  let start_stat = alloc_stats () in

  let localstart = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  let finish_stat = alloc_stats () in

  if Cmdline._trace_progress () then (
    print_string "%%% ";
    Printf.printf "[+%fs = %fs] %s%a\n"
      (finish -. localstart)
      (finish -. start)
      desc
      output_alloc (start_stat, finish_stat);
    flush stdout;
  );

  result


let time desc f x =
  let start_stat = alloc_stats () in

  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  let finish_stat = alloc_stats () in

  if Cmdline._trace_progress () then (
    print_string "%%% ";
    Printf.printf "%s took %fs%a\n"
      desc
      (finish -. start)
      output_alloc (start_stat, finish_stat);
    flush stdout;
  );

  result
