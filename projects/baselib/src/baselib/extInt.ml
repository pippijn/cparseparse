let rec fold f x l h =
  if l == h + 1 then
    x
  else
    fold f (f x l) (l + 1) h
