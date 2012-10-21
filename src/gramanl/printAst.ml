open GrammarAst

let f = Printf.fprintf


let print_termdecl out = function
  | TermDecl (code, name, "") ->
      f out "  %3d : %s;\n" code name
  | TermDecl (code, name, alias) ->
      f out "  %3d : %s \"%s\";\n" code name alias

let print_action_code out = function
  | None ->
      f out ";\n"
  | Some code ->
      f out " %s\n" code

let print_specfunc out = function
  | SpecFunc (name, formals, code) ->
      f out "  fun %s (" name;
      ignore (List.fold_left (fun first formal ->
        if not first then
          output_string out ", ";
        output_string out formal;
        false
      ) true formals);
      f out ") %s\n" code

let print_type out = function
  | "" -> ()
  | ty -> f out "(%s)" ty

let print_termtype out = function
  | TermType (name, termtype, []) ->
      f out "  token%a %s;\n" print_type termtype name
  | TermType (name, termtype, funcs) ->
      f out "  token%a %s {\n" print_type termtype name;
      List.iter (print_specfunc out) funcs;
      f out "}\n"

let print_precspec out = function
  | PrecSpec (kind, prec, tokens) ->
      f out "    %s %d" (Assoc.to_string kind) prec;
      List.iter (f out " %s") tokens;
      f out ";\n"

let print_rhs out = function
  | RH_name ("", name) ->
      f out " %s" name
  | RH_name (tag, name) ->
      f out " %s:%s" tag name
  | RH_string ("", str) ->
      f out " %s" str
  | RH_string (tag, str) ->
      f out " %s:%s" tag str
  | RH_prec (tokName) ->
      f out " prec (%s)" tokName
  | RH_forbid (tokName) ->
      f out " forbid_next (%s)" tokName

let print_proddecl out = function
  | ProdDecl (PDK_NEW, None, rhs, actionCode) ->
      f out "  ->";
      List.iter (print_rhs out) rhs;
      print_action_code out actionCode
  | ProdDecl (PDK_NEW, Some prod_name, rhs, actionCode) ->
      f out "  -> [%s]" prod_name;
      List.iter (print_rhs out) rhs;
      print_action_code out actionCode
  | ProdDecl (PDK_REPLACE, None, rhs, actionCode) ->
      f out "  replace";
      List.iter (print_rhs out) rhs;
      print_action_code out actionCode
  | ProdDecl (PDK_DELETE, None, rhs, actionCode) ->
      f out "  delete";
      List.iter (print_rhs out) rhs;
      print_action_code out actionCode
  | _ -> failwith "invalid ProdDecl"

let print_topform out = function
  | TF_verbatim (false, code) ->
      f out "\nverbatim {\n%s\n}\n" code
  | TF_verbatim (true, code) ->
      f out "\nimpl_verbatim {\n%s\n}\n" code
  | TF_option (name, value) ->
      f out "option %s %d;\n" name value
  | TF_terminals (decls, types, prec) ->
      f out "\nterminals {\n";
      List.iter (print_termdecl out) decls;
      f out "\n";
      List.iter (print_termtype out) types;
      f out "\n  precedence {\n";
      List.iter (print_precspec out) prec;
      f out "  }\n";
      f out "}\n\n"
  | TF_nonterm (name, None, funcs, prods, subsets) ->
      f out "nonterm %s {\n" name;
      List.iter (print_specfunc out) funcs;
      List.iter (print_proddecl out) prods;
      List.iter (f out "  %s\n") subsets;
      f out "}\n\n"
  | TF_nonterm (name, Some semtype, funcs, prods, subsets) ->
      f out "nonterm(%s) %s {\n" semtype name;
      List.iter (print_specfunc out) funcs;
      List.iter (print_proddecl out) prods;
      List.iter (f out "  %s\n") subsets;
      f out "}\n\n"

let print ?(out=stdout) ast =
  List.iter (print_topform out) ast
