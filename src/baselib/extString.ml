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
