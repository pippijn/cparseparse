(* Unfortunately, representation and algorithm tend to get
 * mixed together.  Separating them entirely is possible,
 * but syntactically inconvenient.  Non-mutable members are
 * data for representation of the underlying concept, while
 * mutable members are data created by algorithms manipulating
 * the data.
 *
 * Another measure is I've split all grammar-wide algorithm
 * stuff into GrammarAnalysis (gramanl.ml).  Things should
 * only be put into Grammar if they are directly related
 * to the grammar representation.  (However, constitutent
 * objects like Production will continue to be a mix.)
 *)

open Sexplib.Conv
module Stringmap = Stringmap.M

type spec_func = {
  params 		: string list;
  code   		: string;
} with sexp

type symbol_base = {
  (* --- representation --- *)
  name    		: string; (* symbol's name in grammar *)
  semtype 		: string; (* OCaml type of semantic value *)

  dup     		: spec_func option; (* code to duplicate a semantic value *)
  del     		: spec_func option; (* code to clean up a semantic value *)

  (* --- annotation --- *)
  mutable reachable 	: bool; (* true when symbol reachable from start symbol *)
} with sexp

(* something that only appears on the right-hand side of
 * productions, and is an element of the source language
 * NOTE:  This is really a terminal *class*, in that it's possible
 * for several different tokens to be classified into the same
 * terminal class (e.g. "foo" and "bar" are both identifiers) *)
type terminal = {
  tbase   		: symbol_base;

  (* --- representation --- *)
  (* whereas 'name' is the canonical name for the terminal class,
   * this field is an alias; for example, if the canonical name is
   * L2_EQUALEQUAL, the alias might be "=="; the alias should *not*
   * include actual double-quote characters
   * if the alias is "", there is no alias *)
  alias         	: string;
  (* parsgen-time conflict resolution: if a shift/reduce conflict
   * occurs between a production and a symbol, both with specified
   * precedence (not 0), then the one with the numerically higher
   * precedence will be used *)
  precedence 		: int;
  (* if, in the above scenario, the precedence values are the same,
   * then the associativity kind will be used to decide which to use *)
  associativity		: Assoc.kind;
  (* code to reclassify a token type *)
  classify 		: spec_func option;
  (* terminal class index - this terminal's id; -1 means unassigned *)
  term_index 		: int;
} with sexp

(* something that can appear on the left-hand side of a production
 * (or, empty_nonterminal, since we classify that as a nonterminal also) *)
type nonterminal = {
  nbase 		: symbol_base;

  (* --- representation --- *)
  merge 		: spec_func option; (* code to resolve ambiguities *)
  keep 			: spec_func option; (* code to decide whether to keep a reduction *)

  maximal 		: bool; (* if true, use maximal munch disambiguation *)

  subset_names 		: string list; (* preferred subsets (for scannerless) *)

  (* --- annotation --- *)
  first 		: TerminalSet.t; (* set of terminals that can be start of a string derived from 'this' *)
  follow 		: TerminalSet.t; (* set of terminals that can follow a string derived from 'this' *)
  mutable nt_index 	: int; (* nonterminal index in indexed_nonterminals for grammar analysis *)
  mutable cyclic 	: bool; (* true if this can derive itself in 1 or more steps *)
  mutable subsets	: nonterminal list; (* resolved subsets *)
  mutable superset	: nonterminal option; (* inverse of 'subsets' *)
} with sexp

(* either a nonterminal or terminal symbol *)
type symbol =
  (* tags are applied to the symbols for purposes of unambiguous naming
   * in actions, and for self-commenting value as role indicators; an
   * empty tag ("") is allowed and means there is no tag *)
  | Terminal    of (* tag: *)string * terminal
  | Nonterminal of (* tag: *)string * nonterminal
  with sexp

(* a rewrite rule *)
type production = {
  (* --- representation --- *)
  left 			: nonterminal; (* left hand side *)
  right 		: symbol list; (* right hand side; terminals & nonterminals *)
  prec 			: int; (* precedence level for disambiguation (0 for none specified) *)
  forbid		: TerminalSet.t; (* forbidden next tokens *)

  action 		: string; (* user-supplied reduction action code *)

  (* --- annotation --- *)
  first_rhs 		: TerminalSet.t; (* First(RHS) *)
  mutable prod_index 	: int; (* unique production id *)
} with sexp

type grammar = {
  (* --- representation --- *)
  nonterminals 		: nonterminal Stringmap.t;
  terminals 		: terminal Stringmap.t;
  aliases 		: string Stringmap.t;
  productions 		: production list;
  start_symbol 		: nonterminal;

  (* sections of verbatim code emitted into the interface file, before 
   * the parser context class body *)
  verbatim 		: string list;
  (* code emitted into the implementation file at the end *)
  impl_verbatim 	: string list;

  (* expected numbers of various anomalies; -1 means no
   * expectation has been supplied; this informtion is used
   * to control what is reported after grammar analysis *)
  expectedSR 		: int; (* shift/reduce conflicts *)
  expectedRR 		: int; (* reduce/reduce conflicts *)
  expectedUNRNonterms 	: int; (* # unreachable nonterminals *)
  expectedUNRTerms 	: int; (* # unreachable terminals *)

  (* when true, the default dup/del is what's expected for a
   * garbage-collected system: dup() is the identity function,
   * and del() is a no-op *)
  useGCDefaults 	: bool;
  (* when true, unspecified merge() functions abort the program *)
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


(* the special terminal for the empty string; does not appear in the
 * list of nonterminals or terminals for a grammar, but can be
 * referenced by productions, etc.; the decision to explicitly have
 * such a symbol, instead of letting it always be implicit, is
 * motivated by things like the derivability relation, where it's
 * nice to treat empty like any other symbol *)
let empty_nonterminal = {
  nbase    = { empty_symbol_base with name = "empty" };

  merge    = None;
  keep     = None;

  maximal  = false;

  subset_names = [];

  first    = TerminalSet.empty_set;
  follow   = TerminalSet.empty_set;
  (* empty has an index of 0; all other nonterminals must have indices >= 1 *)
  nt_index = 0;
  cyclic   = false;
  subsets  = [];
  superset = None;
}


let empty_production = {
  left       = empty_nonterminal;
  right      = [];
  prec       = 0;
  forbid     = TerminalSet.empty_set;
            
  action     = "";

  first_rhs  = TerminalSet.empty_set;
  prod_index = -1;
}


let empty_grammar = {
  nonterminals        = Stringmap.empty;
  terminals           = Stringmap.empty;
  aliases             = Stringmap.empty;
  productions         = [];
  start_symbol        = empty_nonterminal;
                     
  verbatim            = [];
  impl_verbatim       = [];

  expectedSR          = 0;
  expectedRR          = 0;
  expectedUNRNonterms = 0;
  expectedUNRTerms    = 0;

  useGCDefaults       = false;
  defaultMergeAborts  = false;
}
