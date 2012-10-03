let iter_until f l =
  List.fold_left (fun finished head ->
    finished || f head
  ) false l


let iterl f l =
  ignore (
    List.fold_left (fun tail head ->
      f tail head;
      List.tl tail
    ) l l
  )

let iterl_until f l =
  fst (
    List.fold_left (fun (finished, tail) head ->
      (finished || f tail head), List.tl tail
    ) (false, l) l
  )


let fold_leftl f x l =
  snd (
    List.fold_left (fun (tail, x) head ->
      List.tl tail, f x tail head
    ) (l, x) l
  )


let fold_leftl_until f x l =
  let _, _, x =

    List.fold_left (fun (finished, tail, x) head ->
      let finished, x =
        if finished then
          (true, x)
        else
          f x tail head
      in
      finished, List.tl tail, x
    ) (false, l, x) l

  in
  x
