let _trace_srcloc = ref false
let _trace_lexing = ref false


let () =
  Cmdline.register "dfa construction" Arg.([
    "-trace-srcloc",		Set _trace_srcloc,		" show source locations for each located lexeme";
    "-trace-lexing",		Set _trace_lexing,		" output each token as it is parsed";
  ])


let _trace_srcloc () = !_trace_srcloc
let _trace_lexing () = !_trace_lexing
