open Camlp4.PreCast
open Jq_ast

module Gram = MakeGram(Lexer)
let json = Gram.Entry.mk "json"
let jlist_rest = Gram.Entry.mk "jlist_rest"


EXTEND Gram
  json: FIRST [[
      "null" -> Jq_null
    | "true" -> Jq_bool true
    | "false" -> Jq_bool false
    | i = INT -> Jq_int (int_of_string i)
    | f = FLOAT -> Jq_float (float_of_string f)
    | s = STRING -> Jq_string s
    | "["; es = LIST0 json SEP ","; s = jlist_rest; "]" -> Jq_array (es, s)
    | "{";
        kvs =
          LIST0
            [ s = STRING; ":"; j = json -> (s, j) ]
            SEP ",";
      "}" -> Jq_object (List.sort (fun (a, _) (b, _) -> String.compare a b) kvs)

    | `ANTIQUOT ("",	  s) -> Jq_Ant (_loc, Aq_none , s)
    | `ANTIQUOT ("alist", s) -> Jq_Ant (_loc, Aq_alist, s)
    | `ANTIQUOT ("list",  s) -> Jq_Ant (_loc, Aq_list , s)
    | `ANTIQUOT ("str",	  s) -> Jq_Ant (_loc, Aq_str  , s)
    | `ANTIQUOT ("flo",	  s) -> Jq_Ant (_loc, Aq_flo  , s)
    | `ANTIQUOT ("int",	  s) -> Jq_Ant (_loc, Aq_int  , s)
    | `ANTIQUOT ("bool",  s) -> Jq_Ant (_loc, Aq_bool , s)
  ]];

  jlist_rest: [[
      ".."; `ANTIQUOT ("", s)	-> s
    | ".."			-> "_"
    | (* empty *)		-> ""
  ]];
END
