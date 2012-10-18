open Typing
open Options

let (|>) = BatPervasives.(|>)
let (-|) = BatPervasives.(-|)
let (|-) = BatPervasives.(|-)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Some good runtime defaults                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let run f =
  Printexc.record_backtrace true;
  Printexc.print f Options.inputs


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
    let program =
      let lexbuf = Lexing.from_channel (open_in name) in
      Parser.program Lexer.token lexbuf in

    if Options._dump_tokens () then (
      Printf.printf "***** Processing %s\n" name;
      begin
        let lexbuf = Lexing.from_channel (open_in name) in
        let tokens = tokenise [] Lexer.token lexbuf in
        List.iter (print_endline -| Token.to_string) tokens;
      end;
      Printf.printf "***** End of %s\n" name
    );

    if Options._dump_ast () then (
      program |> Program.output_untyped_program stdout
    );

    if not (Options._no_emit ()) then (
      program
      |> if Options._special ()
         then SimpleBackend.output_program "/dev/stdout"
         else assert false
    );

    if Options._infer () then (
      List.iter ((|>) program) [
        (new Program.print) # program Format.std_formatter;
        Typing.program |- Program.output_typed_program stdout;
        Typing.program |- (new Program.typed_print)#program Format.std_formatter;
      ]
    )
  in
  List.iter single

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                             Run our tool                              | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let () = Cmdline.run files
