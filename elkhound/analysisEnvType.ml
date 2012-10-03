open Gramtype

type dotted_production = {
  prod : production;
  dot  : int;
  before_dot : symbol option;
  after_dot  : symbol option;
  first_set  : TerminalSet.t;
  mutable can_derive_empty : bool;
}

type env = {
  indexed_nonterms       : nonterminal array;
  indexed_terms          : terminal array;
  indexed_prods          : production array;
  prods_by_lhs           : production list array;
  dotted_prods           : dotted_production array array;
  derivable              : Bit2d.t;
  mutable cyclic_grammar : bool;
}
