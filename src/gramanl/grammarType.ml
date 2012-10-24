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


type spec_func = {
  params                : string list;
  code                  : CamlAst.expr;
} with sexp


(* all semantic actions and type *)
type semantic = [
  | `SEM_TYPE of CamlAst.ctyp (* OCaml type of semantic value *)

  | `SEM_DUP of spec_func (* code to duplicate a semantic value *)
  | `SEM_DEL of spec_func (* code to clean up a semantic value *)
  | `SEM_CLASSIFY of spec_func (* code to reclassify a token type *)
  | `SEM_MERGE of spec_func (* code to resolve ambiguities *)
  | `SEM_KEEP of spec_func (* code to decide whether to keep a reduction *)

  | `SEM_ACTION of CamlAst.expr (* user-supplied reduction action code *)
]

type term_semantic = [
  | `SEM_TYPE of CamlAst.ctyp

  | `SEM_DUP of spec_func
  | `SEM_DEL of spec_func
  | `SEM_CLASSIFY of spec_func
] with sexp

type nonterm_semantic = [
  | `SEM_TYPE of CamlAst.ctyp

  | `SEM_DUP of spec_func
  | `SEM_DEL of spec_func
  | `SEM_MERGE of spec_func
  | `SEM_KEEP of spec_func
] with sexp

type prod_semantic = [
  | `SEM_ACTION of CamlAst.expr
] with sexp

type ('index, 'semantic) symbol_base = {
  (* --- representation --- *)
  name                  : string; (* symbol's name in grammar *)
  index_id		: 'index; (* unique symbol id *)
  semantic		: 'semantic list; (* semantic actions and type *)
} with sexp

(* something that only appears on the right-hand side of
 * productions, and is an element of the source language
 * NOTE:  This is really a terminal *class*, in that it's possible
 * for several different tokens to be classified into the same
 * terminal class (e.g. "foo" and "bar" are both identifiers) *)
type terminal = {
  (* terminal class index - this terminal's id; -1 means unassigned *)
  tbase                 : (Ids.Terminal.t, term_semantic) symbol_base;

  (* --- representation --- *)
  (* whereas 'name' is the canonical name for the terminal class,
   * this field is an alias; for example, if the canonical name is
   * L2_EQUALEQUAL, the alias might be "=="; the alias should *not*
   * include actual double-quote characters
   * if the alias is "", there is no alias *)
  alias                 : string;
  (* parsgen-time conflict resolution: if a shift/reduce conflict
   * occurs between a production and a symbol, both with specified
   * precedence (not 0), then the one with the numerically higher
   * precedence will be used *)
  precedence            : int;
  (* if, in the above scenario, the precedence values are the same,
   * then the associativity kind will be used to decide which to use *)
  associativity         : Assoc.kind;
} with sexp

(* something that can appear on the left-hand side of a production
 * (or, empty_nonterminal, since we classify that as a nonterminal also) *)
type nonterminal = {
  (* nonterminal index in indexed_nonterminals for grammar analysis *)
  nbase                 : (Ids.Nonterminal.t, nonterm_semantic) symbol_base;

  (* --- representation --- *)
  maximal               : bool; (* if true, use maximal munch disambiguation *)

  subset_names          : string list; (* preferred subsets (for scannerless) *)

  (* --- annotation --- *)
  mutable first         : TerminalSet.t; (* set of terminals that can be start of a string derived from 'this' *)
  mutable follow        : TerminalSet.t; (* set of terminals that can follow a string derived from 'this' *)
  mutable cyclic        : bool; (* true if this can derive itself in 1 or more steps *)
  mutable subsets       : Ids.Nonterminal.t list; (* resolved subsets *)
  mutable superset      : Ids.Nonterminal.t option; (* inverse of 'subsets' *)
} with sexp

(* either a nonterminal or terminal symbol *)
type symbol =
  (* tags are applied to the symbols for purposes of unambiguous naming
   * in actions, and for self-commenting value as role indicators; an
   * empty tag ("") is allowed and means there is no tag *)
  | Terminal    of (* tag: *)string * Ids.Terminal.t
  | Nonterminal of (* tag: *)string * Ids.Nonterminal.t
  with sexp

(* a rewrite rule *)
type production = {
  (* --- representation --- *)
  pbase			: (Ids.Production.t, prod_semantic) symbol_base;

  left                  : Ids.Nonterminal.t; (* left hand side *)
  right                 : symbol list; (* right hand side; terminals & nonterminals *)
  prec                  : int; (* precedence level for disambiguation (0 for none specified) *)
  forbid                : TerminalSet.t; (* forbidden next tokens *)

  (* --- annotation --- *)
  mutable first_rhs     : TerminalSet.t; (* First(RHS) *)
} with sexp

type config = {
  (* expected numbers of various anomalies; -1 means no
   * expectation has been supplied; this informtion is used
   * to control what is reported after grammar analysis *)
  expectedSR            : int; (* shift/reduce conflicts *)
  expectedRR            : int; (* reduce/reduce conflicts *)
  expectedUNRNonterms   : int; (* # unreachable nonterminals *)
  expectedUNRTerms      : int; (* # unreachable terminals *)
} with sexp

type index = {
  terms               	: (terminal, Sig.readonly) TermArray.t;   (* term_index -> terminal    *)
  nonterms              : (nonterminal, Sig.readonly) NtArray.t;  (* nt_index   -> nonterminal *)
  prods                 : (production, Sig.readonly) ProdArray.t; (* prod_index -> production  *)
} with sexp

type grammar = {
  (* --- representation --- *)
  nonterminals          : nonterminal StringMap.t;
  terminals             : terminal StringMap.t;
  aliases               : string StringMap.t;
  productions           : production list;
  start_symbol          : string;

  (* sections of verbatim code emitted into the interface file, before 
   * the parser context class body *)
  verbatim              : CamlAst.sig_item list;
  (* code emitted into the implementation file at the end *)
  impl_verbatim         : CamlAst.str_item list;

  config                : config;
} with sexp


let empty_terminal = {
  tbase         = {
    name     = "";
    index_id = Ids.Terminal.default;
    semantic = [];
  };

  alias         = "";
  precedence    = 0;
  associativity = Assoc.AK_NONASSOC;
}


(* the special terminal for the empty string; does not appear in the
 * list of nonterminals or terminals for a grammar, but can be
 * referenced by productions, etc.; the decision to explicitly have
 * such a symbol, instead of letting it always be implicit, is
 * motivated by things like the derivability relation, where it's
 * nice to treat empty like any other symbol *)
let empty_nonterminal = {
  nbase         = {
    name     = "empty";
    (* empty has an index of 0; all other nonterminals must have indices >= 1 *)
    index_id = Ids.Nonterminal.default;
    semantic = [];
  };

  maximal  	= false;

  subset_names 	= [];

  first    	= TerminalSet.empty;
  follow   	= TerminalSet.empty;
  cyclic   	= false;
  subsets  	= [];
  superset 	= None;
}


let empty_production = {
  pbase         = {
    name     = "";
    index_id = Ids.Production.default;
    semantic = [];
  };

  left       = Ids.Nonterminal.default;
  right      = [];
  prec       = 0;
  forbid     = TerminalSet.empty;

  first_rhs  = TerminalSet.empty;
}


let empty_config = {
  expectedSR          = 0;
  expectedRR          = 0;
  expectedUNRNonterms = 0;
  expectedUNRTerms    = 0;
}
