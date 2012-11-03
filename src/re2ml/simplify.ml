open Ast


(* Define a regular expression matching the full character
 * range. This regexp is used for the wildcard expression "_". *)
let full_chr_class = CharClass (CharClass.of_list CharClass.full_chr_list)


(* Simplify a character class containing ranges to one containing all
 * characters in the range, separately.
 *
 * E.g. [0-9] will be transformed into [0123456789].
 *)
let simplify_char_class cc =
  CharClass (CharClass.of_list (CharClass.to_chr_list cc))


(* TODO *)
let simplify_property = function
  | NameProperty (prop, value) ->
      failwith "unsupported: name-property"
  | IntProperty (prop, value) ->
      failwith "unsupported: int-property"


(* Recursively simplify regexps and their sub-expressions. *)
let rec simplify_regexp = function
  (* Sequences and alternations containing only a single sub-regexp
   * will not be produced by the parser, but in case another
   * transformation produced one, we replace it by its only child
   * after resolving it.*)
  | Sequence [regexp]
  | Alternation [regexp] ->
      Diagnostics.warning Sloc.empty_string
        "Sequence or alternation with single element found";
      simplify_regexp regexp

  (* Sequences, alternations, iterations and name bindings are
   * not reduced any further. Their children are recursively
   * simplified. *)
  | Sequence list -> Sequence (List.map simplify_regexp list)
  | Alternation list -> Alternation (List.map simplify_regexp list)
  | Plus re -> Plus (simplify_regexp re)
  | Binding (re, name) -> Binding (simplify_regexp re, name)

  (* The "optional" quantifier is transformed into an alternation
   * with the empty sentence. +)
   *
   * I.e. a? -> (a | ε) *)
  | Question re -> Alternation [epsilon; simplify_regexp re]
  (* The zero-or-many quantifier "*" can be expressed as an
   * alternation of the non-empty quantifier "+" and the empty
   * sentence.
   *
   * I.e. a* -> (a+ | ε) *)
  | Star re -> Alternation [epsilon; Plus (simplify_regexp re)]

  (* TODO *)
  | Quantified (re, low, high) ->
      failwith "unsupported: {n,m} quantifier"

  (* Unicode character properties are handled separately and
   * the produced regexp may contain arbitrary expressions
   * that are subsequently simplified. *)
  | CharProperty prop ->
      simplify_regexp (simplify_property prop)

  (* Character classes are handled by the CharClass module and
   * the resulting expression requires no further simplification.
   * Note that we could express character classes as an alternation
   * over each character, but that would cause the NFA construction
   * to generate $n$ states for $n$ characters in the class, so
   * instead of simplifying it here, we handle it specially in the
   * NFA step. *)
  | CharClass cc ->
      simplify_char_class cc

  (* The "any character" wildcard expression is resolved as a character
   * class containing the full character set. *)
  | AnyChar ->
      full_chr_class

  (* String literals are expressed as sequence of their characters in
   * order. We could do this in the NFA directly, but the performance
   * improvement is so low that it's not worth it. *)
  | String s ->
      Sequence (
        BatString.fold_right (fun c chars ->
          Char (Sloc.at s c) :: chars
        ) (Sloc.value s) []
      )

  (* Single characters and the end-of-file symbol need no further
   * simplification. *)
  | Eof
  | Char _ as atom ->
      atom

  (* The simplifier has no knowledge of aliases, so it is an error if
   * we try to simplify one. *)
  | Lexeme _ ->
      failwith "cannot simplify unresolved alias"


(* Simplify the regular expression inside the rule. The semantic action
 * is not touched. *)
let simplify_rule (Rule (regexp, code)) =
  Rule (simplify_regexp regexp, code)


(* Simplify all rules in a lexer function. *)
let simplify_lexer (Lexer (name, args, rules)) =
  Lexer (name, args, List.map simplify_rule rules)


(* Produce a new program with simplified rules. This function never
 * produces fatal diagnostics. *)
let simplify (Program (pre, aliases, lexers, post)) =
  if aliases != [] then
    failwith "cannot simplify program with unresolved aliases";
  let lexers = List.map simplify_lexer lexers in

  Program (pre, [], lexers, post)
