let _trace_progress = ref false
let _alloc_stats = ref false
let _alloc_timing = ref false


let () =
  Cmdline.register "general" Arg.([
    "-timing",		Set _trace_progress,		" output timing details";
    "-alloc-stats",	Set _alloc_stats,		" print memory allocation statistics";
    "-alloc-timing",	Set _alloc_timing,		" print alloc statistics in timings (implies -timing)";
  ]) ~action:(fun inputs ->
    if !_alloc_timing then
      _trace_progress := true;
  )


let _trace_progress () = !_trace_progress
let _alloc_stats () = !_alloc_stats
let _alloc_timing () = !_alloc_timing
