open Ast


let rec resolve_regexp map = function
  (* replace references by their definition *)
  | Lexeme name ->
      begin try
        LocStringMap.find name map
      with Not_found ->
        failwith (Sloc.value name)
      end

  (* recursively resolve sub-regexps *)
  | AndGrouping list -> AndGrouping (List.map (resolve_regexp map) list)
  | OrGrouping list -> OrGrouping (List.map (resolve_regexp map) list)
  | Question re -> Question (resolve_regexp map re)
  | Star re -> Star (resolve_regexp map re)
  | Plus re -> Plus (resolve_regexp map re)
  | Quantified (re, low, high) -> Quantified (resolve_regexp map re, low, high)
  | Binding (re, name) -> Binding (resolve_regexp map re, name)

  (* atoms need no resolution *)
  | Eof
  | AnyChar
  | Char _
  | String _
  | CharClass _ as atom ->
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
