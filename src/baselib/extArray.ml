let foldl_until f (x : int) l =
  Array.fold_left (fun r e ->
    if r <> x then
      r
    else
      f e
  ) x l


let foldl_untili f (x : int) l =
  fst (Array.fold_left (fun (r, i) e ->
    if r <> x then
      r, i
    else
      f i e, i + 1
  ) (x, 0) l)


let count f l =
  Array.fold_left (fun count e ->
    count + if f e then 1 else 0
  ) 0 l


let sum f l =
  Array.fold_left (fun sum e ->
    sum + f e
  ) 0 l
