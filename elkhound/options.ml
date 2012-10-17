module Priv = struct
  let _module_prefix = ref "Cc"

  let _paranoid = ref false

  let _use_LR0 = ref false
  let _use_SLR1 = ref false
  let _use_LR1 = ref false
  let _use_LALR1 = ref true

  let _graph_grammar = ref false
  let _graph_automaton = ref false
  let _dump_automaton = ref false

  let _use_table_dump = ref true
  let _inline_table_dump = ref true
  let _compress_table_dump = ref true

  let _trace_closure = ref false
  let _trace_lrsets = ref false
  let _trace_prec = ref false
  let _trace_conflict = ref false
  let _trace_table = ref false
  let _trace_reductions = ref false
  let _trace_first = ref false
  let _trace_derivable = ref false
  let _trace_merge = ref false

  let inputs = ref []
end


let () =
  Arg.(parse (align [
    "-module-prefix",		Set_string Priv._module_prefix,	" prefix for generated modules (%Tables, %Actions, ...)";

    "-paranoid",		Set Priv._paranoid,		" do more expensive internal checks";

    "-use-lr0",			Set Priv._use_LR0,		" generate an LR(0) automaton";
    "-use-slr1",		Set Priv._use_SLR1,		" generate an SLR(1) automaton";
    "-use-lr1",			Set Priv._use_LR1,		" generate an LR(1) automaton";
    "-use-lalr1",		Set Priv._use_LALR1,		" generate an LALR(1) automaton";

    "-graph-grammar",		Set Priv._graph_grammar,	" visualise grammar as graph";
    "-graph-automaton",		Set Priv._graph_automaton,	" visualise LR automaton as graph";
    "-dump-automaton",		Set Priv._dump_automaton,	" dump automaton to a file after LR item set construction";

    "-use-table-dump",		Set Priv._use_table_dump,	" load serialised tables instead of emitting arrays in code";
    "-inline-table-dump",	Set Priv._inline_table_dump,	" inline serialised tables into the Tables module as string";
    "-compress-table-dump",	Set Priv._compress_table_dump,	" compress serialised tables with zlib";

    "-trace-closure",		Set Priv._trace_closure,	" output details during LR item set closure";
    "-trace-lrsets",		Set Priv._trace_lrsets,		" output details during LR item set construction";
    "-trace-prec",		Set Priv._trace_prec,		" output details when precedences are used to resolve conflicts";
    "-trace-conflict",		Set Priv._trace_conflict,	" output details during conflict resolution";
    "-trace-table",		Set Priv._trace_table,		" output details during parse table construction";
    "-trace-reductions",	Set Priv._trace_reductions,	" output details on possible reductions for each state";
    "-trace-first",		Set Priv._trace_first,		" output details during First-set computation";
    "-trace-derivable",		Set Priv._trace_derivable,	" output details during derivability relation computation";
    "-trace-merge",		Set Priv._trace_merge,		" output details while merging grammar modules";
  ]) (fun input -> Priv.inputs := input :: !Priv.inputs) "Usage: elkhound [option...] <file...>")


let _module_prefix = !Priv._module_prefix

let _paranoid = !Priv._paranoid

let _use_LR0 = !Priv._use_LR0
let _use_SLR1 = !Priv._use_SLR1
let _use_LR1 = !Priv._use_LR1
let _use_LALR1 = !Priv._use_LALR1

let _graph_grammar = !Priv._graph_grammar
let _graph_automaton = !Priv._graph_automaton
let _dump_automaton = !Priv._dump_automaton

let _use_table_dump = !Priv._use_table_dump
let _inline_table_dump = !Priv._inline_table_dump
let _compress_table_dump = !Priv._compress_table_dump

let _trace_closure = !Priv._trace_closure
let _trace_lrsets = !Priv._trace_lrsets
let _trace_prec = !Priv._trace_prec
let _trace_conflict = !Priv._trace_conflict
let _trace_table = !Priv._trace_table
let _trace_reductions = !Priv._trace_reductions
let _trace_first = !Priv._trace_first
let _trace_derivable = !Priv._trace_derivable
let _trace_merge = !Priv._trace_merge

let inputs = List.rev !Priv.inputs
