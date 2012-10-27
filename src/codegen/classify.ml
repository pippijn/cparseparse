(************************************************
 * :: Identifier checkers
 ************************************************)

let is_lid id =
  assert (id <> "");
  match id.[0] with
  | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_uid id =
  assert (id <> "");
  match id.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false
