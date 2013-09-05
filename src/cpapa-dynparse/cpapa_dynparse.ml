module Parser = struct
  open Ccparse

  let make_lexer token = Lexerint.({
    token;
    index = (fun (t, s, e) -> CcTokens.index t);
    sval  = (fun (t, s, e) -> CcTokens.sval t);
    sloc  = (fun (t, s, e) -> s, e);
  })

  let parse_string code =
    let lexbuf = Lexing.from_string code in

    let lexer = make_lexer Lexer.token in

    let glr = Glr.Engine.create CcActions.userActions CcTables.parseTables in

    Glr.Engine.parse glr lexer lexbuf

end


let title = "C++ parser example"
let missing_title = "Environment (unimplemented)"
let code_title = "C++ code"
let sexp_title = "Parse tree"
let output_title = "Deparsed"

let example = "\
int main ()
{
  return 0;
}"

let lang_mime_type = "text/x-c++"


let parse wcols code sexp output error missing =
  try

    let tu = Parser.parse_string code in
    sexp (Sexplib.Sexp.to_string_hum (Ast.sexp_of_translation_unit tu));

    output ("unimplemented");

  with
  | Glr.Engine.Located ((start_p, end_p), e, extra) ->
    let open Lexing in
    (* print source position *)
    let pos =
      Printf.sprintf "input.cpp:%d:%d: "
        (start_p.pos_lnum)
        (start_p.pos_cnum - start_p.pos_bol + 1)
    in

    (* print exception info *)
    let info =
      match e with
      | Glr.Engine.ParseError (state, token, expected, reason) ->
          Printf.sprintf "parse error near \"%s\" (state: %d, token: %d)"
            extra state token
      | Failure msg ->
          Printf.sprintf "failure in user actions: %s" msg
      | e ->
          Printf.sprintf "exception in user actions: %s"
            (Printexc.to_string e)
    in

    error (pos ^ info)

  | exn ->
      error (
        "Internal error: " ^ Printexc.to_string exn ^ "\n"
        ^ Printexc.get_backtrace ()
      )
