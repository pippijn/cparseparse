open Sexplib.Conv
module Stringmap = Stringmap.M

type spec_func = {
  params 		: string list;
  code   		: string;
} with sexp

type symbol_base = {
  name    		: string;
  semtype 		: string;

  dup     		: spec_func option;
  del     		: spec_func option;

  mutable reachable 	: bool;
} with sexp

type terminal = {
  tbase   		: symbol_base;

  alias         	: string;
  precedence 		: int;
  associativity		: Assoc.kind;
  classify 		: spec_func option;
  term_index 		: int;
} with sexp

type nonterminal = {
  nbase 		: symbol_base;

  merge 		: spec_func option;
  keep 			: spec_func option;

  maximal 		: bool;

  subsets 		: string list;

  first 		: TerminalSet.t;
  follow 		: TerminalSet.t;
  mutable nt_index 	: int;
  mutable cyclic 	: bool;
} with sexp

type rhs_element =
  | Terminal    of string * terminal
  | Nonterminal of string * nonterminal
  with sexp

type production = {
  left 			: nonterminal;
  right 		: rhs_element list;
  prec 			: int;
  forbid		: TerminalSet.t;

  action 		: string;

  first_set 		: TerminalSet.t;
  mutable prod_index 	: int;
} with sexp

type grammar = {
  nonterminals 		: nonterminal Stringmap.t;
  terminals 		: terminal Stringmap.t;
  aliases 		: string Stringmap.t;
  productions 		: production list;
  start_symbol 		: nonterminal;

  verbatim 		: string list;
  impl_verbatim 	: string list;

  expectedSR 		: int;
  expectedRR 		: int;
  expectedUNRNonterms 	: int;
  expectedUNRTerms 	: int;

  useGCDefaults 	: bool;
  defaultMergeAborts 	: bool;
} with sexp


let empty_symbol_base = {
  name      = "";
  semtype   = "";
  dup       = None;
  del       = None;

  reachable = false;
}


let empty_terminal = {
  tbase         = empty_symbol_base;
                
  alias         = "";
  precedence    = 0;
  associativity = Assoc.AK_NONASSOC;
  classify      = None;
  term_index    = -1;
}


let empty_nonterminal = {
  nbase        = { empty_symbol_base with name = "empty" };

  merge        = None;
  keep         = None;

  maximal      = false;

  subsets      = [];

  first        = TerminalSet.empty ();
  follow       = TerminalSet.empty ();
  nt_index     = -1;
  cyclic       = false;
}


let empty_production = {
  left = empty_nonterminal;
  right = [];
  prec = 0;
  forbid = TerminalSet.empty ();

  action = "";

  first_set = TerminalSet.empty ();
  prod_index = -1;
}


let empty_grammar = {
  nonterminals  = Stringmap.empty;
  terminals     = Stringmap.empty;
  aliases       = Stringmap.empty;
  productions   = [];
  start_symbol  = empty_nonterminal;

  verbatim      = [];
  impl_verbatim = [];

  expectedSR          = 0;
  expectedRR          = 0;
  expectedUNRNonterms = 0;
  expectedUNRTerms    = 0;

  useGCDefaults 	     = false;
  defaultMergeAborts 	     = false;
}
