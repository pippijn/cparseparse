include String

let ends_with suffix str =
  let slen = String.length suffix in
  let nlen = String.length str in
  slen <= nlen && String.sub str (nlen - slen) slen = suffix


let starts_with prefix str =
  let plen = String.length prefix in
  let nlen = String.length str in
  plen <= nlen && String.sub str 0 plen = prefix


let without_prefix prefix str =
  if starts_with prefix str then
    let plen = String.length prefix in
    let nlen = String.length str in
    Some (String.sub str plen (nlen - plen))
  else
    None

let without_prefixes prefixes str =
  List.fold_left (fun result prefix ->
    match without_prefix prefix str with
    | Some _ as result -> result
    | None -> result
  ) None prefixes


let without_suffix suffix str =
  if ends_with suffix str then
    let slen = String.length suffix in
    let nlen = String.length str in
    Some (String.sub str 0 (nlen - slen))
  else
    None

let without_suffixes suffixes str =
  List.fold_left (fun result suffix ->
    match without_suffix suffix str with
    | Some _ as result -> result
    | None -> result
  ) None suffixes


type escape_state =
  | NoEscape
  | Escape
  | HexEscape of int * int

let unescape str =
  let state, pos =
    BatString.fold_left (fun (state, pos) c ->
      match c with
      | '"' | '\'' | '\\' when state == Escape -> str.[pos] <- c; NoEscape, pos + 1
      | 't'  when state == Escape -> str.[pos] <- '\t'; NoEscape, pos + 1
      | 'r'  when state == Escape -> str.[pos] <- '\r'; NoEscape, pos + 1
      | 'n'  when state == Escape -> str.[pos] <- '\n'; NoEscape, pos + 1
      | 'v'  when state == Escape -> str.[pos] <- '\x0c'; NoEscape, pos + 1
      | 'f'  when state == Escape -> str.[pos] <- '\x0b'; NoEscape, pos + 1

      | 'x'  when state == Escape -> HexEscape (0, 2), pos

      | '0' .. '9' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char '0' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ ->
              str.[pos] <- c; state, pos + 1
          end

      | 'a' .. 'f' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char 'a' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ -> str.[pos] <- c; state, pos + 1
          end

      | 'A' .. 'F' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char 'A' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ -> str.[pos] <- c; state, pos + 1
          end

      | c when state == Escape ->
          failwith ("Unknown escape sequence: " ^ Char.escaped c)

      | '\\' -> Escape, pos
      | c -> str.[pos] <- c; NoEscape, pos + 1
    ) (NoEscape, 0) str
  in

  let pos =
    match state with
    | NoEscape -> pos
    | Escape -> failwith "unterminated escape sequence"
    | HexEscape (i, 0) -> str.[pos] <- char_of_int i; pos + 1
    | HexEscape (i, l) -> failwith "unterminated hex escape"
  in

  String.sub str 0 pos
