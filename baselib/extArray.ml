let foldl_until f x l =
  Array.fold_left (fun r e ->
    if r <> x then
      r
    else
      f e
  ) x l
