module State : sig
  include GrammarSig.IntegralModuleType

  val is_start : t -> bool
end


(* nonterminals *)
module Nonterminal : sig
  include GrammarSig.IntegralModuleType

  val empty : t
  val is_empty : t -> bool

  val start : t
  val is_start : t -> bool
end
