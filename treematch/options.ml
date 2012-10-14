module Priv = struct
  let _i = ref false
  let _infer = ref false
  let _dump_ast = ref false
  let inputs = ref []
end

let () =
  Arg.(parse (align [
    "-i",                       Set Priv._i,		" generate interface files";
    "-infer",                   Set Priv._infer,        " infer the ast from map strategies";
    "-dump-ast",		Set Priv._dump_ast,	" dump source ast";
  ]) (fun input -> Priv.inputs := input :: !Priv.inputs) "Usage: treematch [option...] <file...>")

let _i = !Priv._i
let _infer = !Priv._infer
let _dump_ast = !Priv._dump_ast

let inputs = !Priv.inputs
