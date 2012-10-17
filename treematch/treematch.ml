open Typing
let (|>) = BatPervasives.(|>)
let (-|) = BatPervasives.(-|)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Some good runtime defaults                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let run f =
  Printexc.record_backtrace true;
  Printexc.print f Options.inputs

open Options

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                         Return list of tokens                         | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let rec tokenise tokens token lexbuf =
  let next = token lexbuf in
  if next == Parser.TOK_EOF then (
    if false then (
      Printf.printf "%d tokens\n" (List.length tokens);
      flush stdout;
    );
    List.rev (next :: tokens)
  ) else (
    tokenise (next :: tokens) token lexbuf
  )

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Process files                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)


let files =
  let single name =
    let parse () =
      let lexbuf = Lexing.from_channel (open_in name) in
      Parser.program Lexer.token lexbuf in
    if Options._dump_tokens then (
      Printf.printf "***** Processing %s\n" name;
      let lexbuf = Lexing.from_channel (open_in name) in
      let tokens = tokenise [] Lexer.token lexbuf in
      List.iter (print_endline -| Token.to_string) tokens;
      Printf.printf "***** End of %s\n" name;
      flush stdout);
    if Options._dump_ast then (
      parse () |> Program.output_untyped_program stdout
    );
    if not Options._no_emit then (
      parse () |> Backend.output_program "/dev/stdout"
    );
    if Options._infer then (
      let p = new Program.print in
      let program = parse () in
      program |> p # program Format.std_formatter;
      Typing.Collect.print Format.std_formatter "ListB" program;
      program |> Typing.Annotate.program |> Program.output_typed_program stdout;
      ()
    )
  in
  List.iter single

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Run our tool                              | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let () = run files
