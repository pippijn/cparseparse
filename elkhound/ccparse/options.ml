module Priv = struct
  let _ptree = ref false
  let _print = ref false
  let _utf8 = ref false
  let _pp = ref false
  let _tokens = ref false
  let _dumptoks = ref false
  let _loadtoks = ref false
  let _stats = ref false
  let _trivial = ref false
  let _timing = ref false

  let inputs = ref []
end

let () =
  Arg.(parse (align [
    "-ptree",		Set Priv._ptree,	" build parse tree";
    "-print",		Set Priv._print,	" print tree";
    "-utf8",		Set Priv._utf8,		" assume source file is in UTF-8 encoding";
    "-pp",		Set Priv._pp,		" fully tokenise before parsing";
    "-tokens",		Set Priv._tokens,	" tokenise only; do not parse";
    "-dumptoks",	Set Priv._dumptoks,	" dump tokens to file (implies -pp)";
    "-loadtoks",	Set Priv._loadtoks,	" load tokens from file";
    "-stats",		Set Priv._stats,	" print parsing statistics";
    "-trivial",		Set Priv._trivial,	" use trivial user actions";
    "-timing",		Set Priv._timing,	" output timing details";
  ]) (fun input -> Priv.inputs := input :: !Priv.inputs) "Usage: cxxparse [option...] <file...>");

  if !Priv._dumptoks then
    Priv._pp := true


let _ptree = !Priv._ptree
let _print = !Priv._print
let _utf8 = !Priv._utf8
let _pp = !Priv._pp
let _tokens = !Priv._tokens
let _dumptoks = !Priv._dumptoks
let _loadtoks = !Priv._loadtoks
let _stats = !Priv._stats
let _trivial = !Priv._trivial
let _timing = !Priv._timing

let inputs = !Priv.inputs
