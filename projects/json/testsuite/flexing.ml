open Lexing

type lexbuf = Lexing.lexbuf =
  { refill_buff : lexbuf -> unit;
    mutable lex_buffer : string;
    mutable lex_buffer_len : int;
    mutable lex_abs_pos : int;
    mutable lex_start_pos : int;
    mutable lex_curr_pos : int;
    mutable lex_last_pos : int;
    mutable lex_last_action : int;
    mutable lex_eof_reached : bool;
    mutable lex_mem : int array;
    mutable lex_start_p : position;
    mutable lex_curr_p : position;
  }

let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

let from_string lex_buffer = {
  refill_buff = (fun lexbuf -> lexbuf.lex_eof_reached <- true);
  lex_buffer;
  lex_buffer_len = String.length lex_buffer;
  lex_abs_pos = 0;
  lex_start_pos = 0;
  lex_curr_pos = 0;
  lex_last_pos = 0;
  lex_last_action = 0;
  lex_mem = [||];
  lex_eof_reached = false;
  lex_start_p = zero_pos;
  lex_curr_p = zero_pos;
}


let from_file file =
  match TestFramework.slurp (open_in file) with
  | [s] -> from_string s
  | l   -> from_string (String.concat "" l)
