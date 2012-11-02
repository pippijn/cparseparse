open Ast


let full_chr_range =
  CharClass (Positive (List.rev_map (fun c -> Single c) CharClass.full_chr_list))


let resolve_char_class cc =
  let cc = CharClass.to_chr_list cc in
  CharClass (Positive (List.rev_map (fun c -> Single c) cc))


let resolve_property = function
  | NameProperty (prop, value) ->
      failwith "unsupported: name-property"
  | IntProperty (prop, value) ->
      failwith "unsupported: int-property"


let rec resolve_regexp map = function
  (* replace references by their definition *)
  | Lexeme name ->
      begin try
        LocStringMap.find name map
      with Not_found ->
        let alias = Sloc.value name in
        let what =
          if alias.[0] == '\\' then
            "builtin regex"
          else
            "regex alias"
        in
        Diagnostics.error name "No such %s: '%s'" what alias;
        (* error recovery *)
        epsilon
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
  | CharProperty prop ->
      resolve_regexp map (resolve_property prop)

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


let resolve_list =
  List.fold_left (fun map (name, regexp) ->
    LocStringMap.add
      (Sloc.generated name)
      (resolve_regexp map (StringParser.regexp regexp))
      map
  )


let classes = resolve_list LocStringMap.empty [
  (* Digits *)
  "[:digit:]",	"['0'-'9']";
  (* Hexadecimal digits *)
  "[:xdigit:]",	"['A'-'F' 'a'-'f' '0'-'9']";
  (* Lowercase letters *)
  "[:lower:]",	"['a'-'z']";
  (* Uppercase letters *)
  "[:upper:]",	"['A'-'Z']";
  (* Visible characters *)
  "[:graph:]",	"['\\x21'-'\\x7e']";
  (* Control characters *)
  "[:cntrl:]",	"['\\x00'-'\\x1f' '\\x7f']";
  (* Visible characters and the space character *)
  "[:print:]",	"['\\x20'-'\\x7e']";
  (* Alphanumeric characters plus "_" *)
  "[:word:]",	"['a'-'z' 'A'-'Z' '0'-'9' '_']";
  (* Whitespace characters *)
  "[:space:]",	"[' ' '\\t' '\\r' '\\n' '\\v' '\\f']";
  (* Punctuation characters *)
  "[:punct:]",	"[']' '[' '!' '\"' '#' '$' '%' '&' '\\'' '(' ')' '*' '+' ',' '.' '/' ':' ';' '<' '=' '>' '?' '@' '^' '_' '`' '{' '|' '}' '~' '-']";
]


let builtins = resolve_list classes [
  (* Digits *)
  "\\d",	"[:digit:]";
  (* Non-digits *)
  "\\D",	"[^'0'-'9']";
  (* Hexadecimal digits *)
  "\\x",	"[:xdigit:]";
  (* Lowercase letters *)
  "\\l",	"[:lower:]";
  (* Uppercase letters *)
  "\\u",	"[:upper:]";
  (* Visible characters and the space character *)
  "\\p",	"[:print:]";
  (* Alphanumeric characters plus "_" *)
  "\\w",	"[:word:]";
  (* Non-word characters *)
  "\\W",	"[^'a'-'z' 'A'-'Z' '0'-'9' '_']";
  (* Whitespace characters *)
  "\\s",	"[:space:]";
  (* Non-whitespace characters *)
  "\\S",	"[^' ' '\\t' '\\r' '\\n' '\\v' '\\f']";
]


let resolve_aliases aliases =
  let map =
    List.fold_left (fun map (Alias (name, regexp)) ->
      let regexp = resolve_regexp map regexp in

      LocStringMap.add name regexp map
    ) builtins aliases
  in

  map


let resolve (Program (pre, aliases, lexers, post)) =
  let map = resolve_aliases aliases in
  let lexers = List.map (resolve_lexer map) lexers in

  Diagnostics.exit_on_error ();

  Program (pre, [], lexers, post)
