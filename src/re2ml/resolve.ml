open Ast


(* Recursively resolve aliases in all sub-expressions. *)
let rec resolve_regexp map = function
  (* Replace references by their definition. *)
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
        (* We just return the empty sentence here, so
         * we can catch more undefined alias errors. *)
        epsilon
      end

  (* a b *)
  | Sequence    list -> Sequence    (List.map (resolve_regexp map) list)
  (* a | b *)
  | Alternation list -> Alternation (List.map (resolve_regexp map) list)
  (* a? *)
  | Question re -> Question (resolve_regexp map re)
  (* a{n,m} *)
  | Quantified (re, lo, hi) -> Quantified (resolve_regexp map re, lo, hi)
  (* a+ *)
  | Plus re -> Plus (resolve_regexp map re)
  (* a* *)
  | Star re -> Star (resolve_regexp map re)

  (* a as name *)
  | Binding (re, name) -> Binding (resolve_regexp map re, name)

  (* These expressions do not contain sub-expressions, so they require
   * no name resolution. *)
  | AnyChar | Eof | CharClass _ | CharProperty _ | String _ | Char _ as atom ->
      atom


(* Resolve aliases in all rules of a lexer function. *)
let resolve_lexer map (Lexer (name, args, rules)) =
  let rules =
    List.map (fun (Rule (regexp, code)) ->
      Rule (resolve_regexp map regexp, code)
    ) rules
  in

  Lexer (name, args, rules)


(* [add_builtins map list] adds a list pre-defined aliases to the passed
 * map. It takes a list of pairs (name, code) and parses the code into
 * a regexp object. *)
let add_builtins =
  List.fold_left (fun map (name, regexp) ->
    LocStringMap.add
      (Sloc.generated name)
      (resolve_regexp map (StringParser.regexp regexp))
      map
  )


(* POSIX character classes *)
let classes = add_builtins LocStringMap.empty [
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


(* Some common short-cuts for the above character classes. *)
let builtins = add_builtins classes [
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


(* Collect regexp aliases and resolve them using the ones already seen.
 * If an alias is defined twice, replace it in the map and emit a warning.
 * This function returns a map of built-in and user-defined aliases to
 * their regexp objects. *)
let resolve_aliases =
  List.fold_left (fun map (Alias (name, regexp)) ->
    let regexp = resolve_regexp map regexp in

    (* We allow redefinition as in ML, but warn about it, since it is
     * probably not what the user wants. *)
    if LocStringMap.mem name map then
      Diagnostics.warning name "regexp name %a already defined" Sloc.pp_print_string name;
    LocStringMap.add name regexp map
  ) builtins


(* Produce a program with resolved aliases and an empty aliases list. *)
let resolve (Program (pre, aliases, lexers, post)) =
  let map = resolve_aliases aliases in
  let lexers = List.map (resolve_lexer map) lexers in

  (* If there was an error resolving an alias, raise 
   * the Diagnostics.Exit exception. *)
  Diagnostics.exit_on_error ();

  Program (pre, [], lexers, post)
