let _ptree = ref false
let _tptree = ref false
let _treematch = ref false
let _print = ref false
let _pp = ref false
let _tokens = ref false
let _dumptoks = ref false
let _loadtoks = ref false
let _stats = ref false
let _trivial = ref false
let _xc = ref false
let _rt = ref false


let () =
  Cmdline.register Arg.([
    "-ptree",		Set _ptree,		" build parse tree";
    "-tptree",		Set _tptree,		" build strongly typed parse tree";
    "-treematch",	Set _treematch,		" build treematch-backed tree";
    "-print",		Set _print,		" print tree";
    "-pp",		Set _pp,		" fully tokenise before parsing";
    "-tokens",		Set _tokens,		" tokenise only; do not parse (implies -pp)";
    "-dumptoks",	Set _dumptoks,		" dump tokens to file (implies -pp)";
    "-loadtoks",	Set _loadtoks,		" load tokens from file";
    "-stats",		Set _stats,		" print parsing statistics";
    "-trivial",		Set _trivial,		" use trivial user actions";
    "-xc",		Set _xc,		" parse code as C, not as C++ (implicit if any input file name ends with .c)";
    "-rt",		Set _rt,		" set real-time scheduling policy with highest priority";
  ]) ~action:(fun inputs ->
    if !_dumptoks || !_tokens then
      _pp := true;

    if List.filter (ExtString.ends_with ".c") inputs <> [] then
      _xc := true;
  )


let _ptree () = !_ptree
let _tptree () = !_tptree
let _treematch () = !_treematch
let _print () = !_print
let _pp () = !_pp
let _tokens () = !_tokens
let _dumptoks () = !_dumptoks
let _loadtoks () = !_loadtoks
let _stats () = !_stats
let _trivial () = !_trivial
let _xc () = !_xc
let _rt () = !_rt
