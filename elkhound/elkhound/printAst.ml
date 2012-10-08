open GrammarAst
open Printf

let print_termdecl = function
  | TermDecl (code, name, "") ->
      printf "  %3d : %s;\n" code name
  | TermDecl (code, name, alias) ->
      printf "  %3d : %s %s;\n" code name alias

let print_specfunc = function
  | SpecFunc (name, formals, code) ->
      printf "  fun %s (" name;
      List.fold_left (fun first formal ->
        if not first then
          print_string ", ";
        print_string formal;
        false
      ) true formals;
      printf ") %s\n" code

let print_termtype = function
  | TermType (name, termtype, []) ->
      printf "  token %s %s;\n" termtype name
  | TermType (name, termtype, funcs) ->
      printf "  token %s %s {\n" termtype name;
      List.iter print_specfunc funcs;
      printf "}\n"

let print_precspec = function
  | PrecSpec (kind, prec, tokens) ->
      printf "    %s %d" (Assoc.to_string kind) prec;
      List.iter (printf " %s") tokens;
      printf ";\n"

let print_rhs = function
  | RH_name ("", name) ->
      printf " %s" name
  | RH_name (tag, name) ->
      printf " %s:%s" tag name
  | RH_string ("", str) ->
      printf " %s" str
  | RH_string (tag, str) ->
      printf " %s:%s" tag str
  | RH_prec (tokName) ->
      printf " prec (%s)" tokName
  | RH_forbid (tokName) ->
      printf " forbid_next (%s)" tokName

let print_proddecl = function
  | ProdDecl (PDK_NEW, rhs, actionCode) ->
      printf "  ->";
      List.iter print_rhs rhs;
      printf " %s\n" actionCode
  | ProdDecl (PDK_REPLACE, rhs, actionCode) ->
      printf "  replace";
      List.iter print_rhs rhs;
      printf " %s\n" actionCode
  | ProdDecl (PDK_DELETE, rhs, actionCode) ->
      printf "  delete";
      List.iter print_rhs rhs;
      printf " %s\n" actionCode

let print_topform = function
  | TF_verbatim (false, code) ->
      printf "\nverbatim%s\n" code
  | TF_verbatim (true, code) ->
      printf "\nimpl_verbatim%s\n" code
  | TF_option (name, value) ->
      printf "option %s %d;\n" name value
  | TF_terminals (decls, types, prec) ->
      printf "\nterminals {\n";
      List.iter print_termdecl decls;
      List.iter print_termtype types;
      printf "\n  precedence {\n";
      List.iter print_precspec prec;
      printf "  }\n";
      printf "}\n\n"
  | TF_nonterm (name, "", funcs, prods, subsets) ->
      printf "nonterm %s {\n" name;
      List.iter print_specfunc funcs;
      List.iter print_proddecl prods;
      List.iter (printf "  %s\n") subsets;
      printf "}\n\n"
  | TF_nonterm (name, semtype, funcs, prods, subsets) ->
      printf "nonterm(%s) %s {\n" semtype name;
      List.iter print_specfunc funcs;
      List.iter print_proddecl prods;
      List.iter (printf "  %s\n") subsets;
      printf "}\n\n"

let print ast =
  List.iter print_topform ast
