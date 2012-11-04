let _dot = ref false
let _dump_automata = ref false

let _auto_loc = ref false

let _trace_srcloc = ref false
let _trace_lexing = ref false


let () =
  Cmdline.register "lexer generation" Arg.([
    "-dot",			Set _dot,			" create graphs for each NFA and DFA";
    "-dump-automata",		Set _dump_automata,		" dump NFA and DFA to stdout";

    "-auto-loc",		Set _auto_loc,			" add code to handle new-lines and keep source locations up-to-date";

    "-trace-srcloc",		Set _trace_srcloc,		" show source locations for each located lexeme";
    "-trace-lexing",		Set _trace_lexing,		" output each token as it is parsed";
  ])


let _dot () = !_dot
let _dump_automata () = !_dump_automata

let _auto_loc () = !_auto_loc

let _trace_srcloc () = !_trace_srcloc
let _trace_lexing () = !_trace_lexing
