include String

let ends_with suffix str =
  let slen = String.length suffix in
  let nlen = String.length str in
  slen <= nlen && String.sub str (nlen - slen) slen = suffix


let starts_with prefix str =
  let plen = String.length prefix in
  let nlen = String.length str in
  plen <= nlen && String.sub str 0 (nlen - plen) = prefix


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
  | HexEscape of int

let unescape str =
  let state, chars =
    BatString.fold_left (fun (state, chars) -> function
      | '"'  when state == Escape -> NoEscape, '"'  :: chars
      | '\'' when state == Escape -> NoEscape, '\'' :: chars
      | '\\' when state == Escape -> NoEscape, '\\' :: chars
      | 'n'  when state == Escape -> NoEscape, '\n' :: chars
      | 't'  when state == Escape -> NoEscape, '\t' :: chars
      | 'r'  when state == Escape -> NoEscape, '\r' :: chars

      | 'x'  when state == Escape -> HexEscape 0, chars
      | '0' .. '9' as c ->
          begin match state with
          | HexEscape i ->
              let m = int_of_char c - int_of_char '0' in
              HexEscape (i * 16 + m), chars
          | _ -> state, c :: chars
          end

      | c    when state == Escape -> failwith ("Unknown escape sequence: " ^ Char.escaped c)

      | '\\' -> Escape, chars
      | c -> NoEscape, c :: chars
    ) (NoEscape, []) str
  in

  let chars =
    match state with
    | NoEscape -> chars
    | Escape -> failwith "unterminated escape sequence"
    | HexEscape i -> char_of_int i :: chars
  in

  BatString.of_list (List.rev chars)
