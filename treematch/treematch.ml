(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Some good runtime defaults                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let (|-) f g x = f (g x)

let run f =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };
  Printexc.record_backtrace true;
  Printexc.print f Options.inputs

open Options

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

let files =
  let single name =
    Printf.printf "***** Processing %s\n" name;
    let lexbuf = Lexing.from_channel (open_in name) in
    let tokens = tokenise [] Lexer.token lexbuf in
    List.iter (print_endline |- Token.to_string) tokens;
    Printf.printf "***** End of %s\n" name;
    flush stdout
  in
  List.iter single

let () = run files
