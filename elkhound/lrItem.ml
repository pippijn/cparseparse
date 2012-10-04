open AnalysisEnvType

(************************************************************
 * :: Functions
 ************************************************************)


(* this returns a dummy item; it allocates the bitmap for 'lookahead',
 * but those bits and the 'dprod' pointer will be overwritten many
 * times during the algorithm *)
let empty term_count = {
  dprod = empty_dotted_production;
  lookahead = TerminalSet.create term_count;
}


let is_dot_at_start item =
  DottedProduction.is_dot_at_start item.dprod


let is_dot_at_end item =
  DottedProduction.is_dot_at_end item.dprod
