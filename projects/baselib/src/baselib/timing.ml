(************************************************************
 * :: Allocation statistics for functions
 ************************************************************)


let output_memsize out size =
  if abs_float size > 1024.0 *. 1024.0 *. 1024.0 then
    Printf.fprintf out "%.1fG" (size /. 1024.0 /. 1024.0 /. 1024.0)
  else if abs_float size > 1024.0 *. 1024.0 then
    Printf.fprintf out "%.1fM" (size /. 1024.0 /. 1024.0)
  else if abs_float size > 1024.0 then
    Printf.fprintf out "%.1fK" (size /. 1024.0)
  else
    Printf.fprintf out "%dB" (int_of_float size)


let output_alloc out ((start, start_bytes), (finish, finish_bytes)) =
  Printf.fprintf out " (live memory %a, allocated %a)"
    output_memsize (float_of_int Gc.((finish.live_words - start.live_words) * Sys.word_size))
    output_memsize (finish_bytes -. start_bytes)


let output_maybe out = function
  | Some a, Some b ->
      output_alloc out (a, b)
  | _ ->
      ()


let alloc_stats () =
  if BaseOptions._alloc_stats () then
    Gc.full_major ();
  Gc.stat (), Gc.allocated_bytes ()


let alloc desc f x =
  let start = alloc_stats () in
  let result = f x in
  let finish = alloc_stats () in

  if BaseOptions._alloc_stats () then (
    print_string "%%% ";
    Printf.printf "%s%a\n"
      desc
      output_alloc (start, finish);
    flush stdout;
  );

  result


(************************************************************
 * :: Timing of functions
 ************************************************************)

let start = Unix.gettimeofday ()


let maybe_alloc_stats alloc =
  if alloc && BaseOptions._alloc_timing () then
    Some (alloc_stats ())
  else
    None


let progress ?(alloc=false) desc f x =
  let start_alloc = maybe_alloc_stats alloc in

  let localstart = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  let finish_alloc = maybe_alloc_stats alloc in

  if BaseOptions._trace_progress () then (
    print_string "%%% ";
    Printf.printf "[+%fs = %fs] %s%a\n"
      (finish -. localstart)
      (finish -. start)
      desc
      output_maybe (start_alloc, finish_alloc);
    flush stdout;
  );

  result


let time ?(alloc=false) desc f x =
  let start_alloc = maybe_alloc_stats alloc in

  let start = Unix.gettimeofday () in
  let result = f x in
  let finish = Unix.gettimeofday () in

  let finish_alloc = maybe_alloc_stats alloc in

  if BaseOptions._trace_progress () then (
    print_string "%%% ";
    Printf.printf "%s took %fs%a\n"
      desc
      (finish -. start)
      output_maybe (start_alloc, finish_alloc);
    flush stdout;
  );

  result
