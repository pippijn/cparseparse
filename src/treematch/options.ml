module Priv = struct
  let _no_emit = ref false
  let _i = ref false
  let _infer = ref false
  let _dump_ast = ref false
  let _dump_tokens = ref false
  let inputs = ref []
end

let () =
  Arg.(parse (align [
    "-no-emit",                 Set Priv._i,		" Don't generate code";
    "-i",                       Set Priv._i,		" generate interface files";
    "-infer",                   Set Priv._infer,        " infer the ast from map strategies";
    "-dump-ast",		Set Priv._dump_ast,	" dump source ast";
    "-dump-tokens",		Set Priv._dump_tokens,	" dump source tokens";
  ]) (fun input -> Priv.inputs := input :: !Priv.inputs) "Usage: treematch [option...] <file...>")

let _no_emit = !Priv._no_emit
let _i = !Priv._i
let _infer = !Priv._infer
let _dump_ast = !Priv._dump_ast
let _dump_tokens =  !Priv._dump_tokens

let inputs = List.rev !Priv.inputs