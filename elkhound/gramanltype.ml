open Gramtype

type dotted_production = {
  prod : production;
  dot  : int;
  before_dot : symbol option;
  after_dot  : symbol option;
}

type env = {
  nonterms               : nonterminal array;
  terms                  : terminal array;
  prods                  : production array;
  prods_by_lhs           : production list array;
  dotted_prods           : dotted_production array array;
  derivable              : Bit2d.t;
  mutable cyclic_grammar : bool;
}
