let _dot = ref false
let _dump_automata = ref false

let _trace_srcloc = ref false
let _trace_lexing = ref false


let () =
  Cmdline.register "dfa construction" Arg.([
    "-dot",			Set _dot,			" create graphs for each NFA and DFA";
    "-dump-automata",		Set _dump_automata,		" dump NFA and DFA to stdout";

    "-trace-srcloc",		Set _trace_srcloc,		" show source locations for each located lexeme";
    "-trace-lexing",		Set _trace_lexing,		" output each token as it is parsed";
  ])


let _dot () = !_dot
let _dump_automata () = !_dump_automata

let _trace_srcloc () = !_trace_srcloc
let _trace_lexing () = !_trace_lexing
