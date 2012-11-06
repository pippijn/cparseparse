let trace_lexing = false;;
let advance lexbuf = let open Lexing
  in
    if lexbuf.lex_eof_reached
    then lexbuf
    else (lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1; lexbuf);;
let curr_char lexbuf state = let open Lexing
  in
    if lexbuf.lex_eof_reached
    then '\000'
    else
      (if trace_lexing
       then
         Printf.printf "state %3d: process char %d (%d-%d / %d) '%s'
" state
           lexbuf.lex_abs_pos lexbuf.lex_start_pos lexbuf.lex_curr_pos
           lexbuf.lex_buffer_len
           (Char.escaped lexbuf.lex_buffer.[lexbuf.lex_curr_pos])
       else ();
       String.unsafe_get lexbuf.lex_buffer lexbuf.lex_curr_pos);;
let curr_char lexbuf state = let open Lexing
  in
    (if lexbuf.lex_curr_pos == lexbuf.lex_buffer_len
     then
       (if trace_lexing then print_endline "[1;33mrefill[0m" else ();
        lexbuf.refill_buff lexbuf)
     else ();
     curr_char lexbuf state);;
let accept lexbuf action = let open Lexing
  in
    (if trace_lexing
     then
       if lexbuf.lex_eof_reached
       then Printf.printf "[1;32maccept at eof: %d[0m
" action
       else
         Printf.printf "[1;32maccept %d-%d '%s': %d[0m
"
           lexbuf.lex_start_pos (lexbuf.lex_curr_pos - 1)
           (Lexing.lexeme lexbuf) action
     else ();
     action);;
let reject lexbuf = let open Lexing
  in
    (if trace_lexing
     then Printf.printf "[1;31mreject at %d[0m
" lexbuf.lex_curr_pos
     else ();
     (-1));;
module Dfa_token =
  struct
    let rec state_0 lexbuf =
      match curr_char lexbuf 0 with
      | 'a' -> state_1 (advance lexbuf)
      | 'b' -> state_2 (advance lexbuf)
      | 'c' -> state_3 (advance lexbuf)
      | _ -> accept lexbuf 0
    and state_1 lexbuf =
      match curr_char lexbuf 1 with
      | 'a' -> state_1 (advance lexbuf)
      | 'b' -> state_2 (advance lexbuf)
      | 'c' -> state_3 (advance lexbuf)
      | _ -> accept lexbuf 0
    and state_2 lexbuf =
      match curr_char lexbuf 2 with
      | 'b' -> state_2 (advance lexbuf)
      | _ -> accept lexbuf 1
    and state_3 lexbuf =
      match curr_char lexbuf 3 with
      | 'c' -> state_3 (advance lexbuf)
      | _ -> accept lexbuf 2;;
  end;;
let error lexbuf =
  if lexbuf.Lexing.lex_eof_reached
  then raise End_of_file
  else failwith (Lexing.lexeme lexbuf);;
let rec token_action lexbuf action =
  match action with
  | 0 -> "a*"
  | 1 -> "a*b*"
  | 2 -> "a*c*"
  | _ -> error lexbuf
and token lexbuf =
  (let open Lexing
   in lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
   let action = Dfa_token.state_0 lexbuf in token_action lexbuf action);;
