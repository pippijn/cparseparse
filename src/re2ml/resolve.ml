open Ast


let full_chr_range =
  CharClass (Positive (List.rev_map (fun c -> Single c) CharClass.full_chr_list))


let resolve_char_class cc =
  let cc = CharClass.to_chr_list cc in
  CharClass (Positive (List.rev_map (fun c -> Single c) cc))


let rec resolve_regexp map = function
  (* replace references by their definition *)
  | Lexeme name ->
      begin try
        LocStringMap.find name map
      with Not_found ->
        failwith (Sloc.value name)
      end

  (* recursively resolve sub-regexps *)
  | Sequence [regexp]
  | OrGrouping [regexp] -> resolve_regexp map regexp

  | Sequence list -> Sequence (List.map (resolve_regexp map) list)
  | OrGrouping list -> OrGrouping (List.map (resolve_regexp map) list)
  | Plus re -> Plus (resolve_regexp map re)
  | Binding (re, name) -> Binding (resolve_regexp map re, name)

  (* a? -> (a | ε) *)
  | Question re -> OrGrouping [epsilon; resolve_regexp map re]
  (* a* -> (a+ | ε) *)
  | Star re -> OrGrouping [epsilon; Plus (resolve_regexp map re)]

  | Quantified (re, low, high) ->
      failwith "unsupported: {n,m} quantifier"

  (* character classes *)
  | CharClass cc ->
      (* no need to further resolve these *)
      resolve_char_class cc

  (* resolve "any character" as the full range *)
  | AnyChar ->
      full_chr_range

  (* these atoms need no resolution *)
  | Eof
  | String _
  | Char _ as atom ->
      atom


let resolve_rule map (Rule (regexp, code)) =
  Rule (resolve_regexp map regexp, code)


let resolve_lexer map (Lexer (name, args, rules)) =
  Lexer (name, args, List.rev (List.rev_map (resolve_rule map) rules))


let resolve_aliases aliases =
  let map =
    List.fold_left (fun map (Alias (name, regexp)) ->
      let regexp = resolve_regexp map regexp in

      LocStringMap.add name regexp map
    ) LocStringMap.empty aliases
  in

  map


let resolve (Program (pre, aliases, lexers, post)) =
  let map = resolve_aliases aliases in
  let lexers = List.map (resolve_lexer map) lexers in

  Program (pre, [], lexers, post)
