let iter_until f l =
  List.fold_left (fun finished head ->
    finished || f head
  ) false l


let iter2_until f l1 l2 =
  List.fold_left2 (fun finished head1 head2 ->
    finished || f head1 head2
  ) false l1 l2


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
      let tail = List.tl tail in
      tail, f x tail head
    ) (l, x) l
  )


let fold_leftl_until f x l =
  let _, _, x =

    List.fold_left (fun (finished, tail, x) head ->
      let tail = List.tl tail in
      let finished, x =
        if finished then
          (true, x)
        else
          f x tail head
      in
      finished, tail, x
    ) (false, l, x) l

  in
  x


let fold_left_many f x ll =
  List.fold_left (List.fold_left f) x ll


let exists_many f ll =
  List.exists (List.exists f) ll


let rec nth_tl l i =
  if i = 0 then
    l
  else
    nth_tl (List.tl l) (i - 1)


let rec foldl_until f x l =
  match l with
  | [] -> x
  | hd :: tl ->
      let r = f hd in
      if r <> x then
        r
      else
        foldl_until f r tl

let rec foldl_until2 f x l1 l2 =
  match l1, l2 with
  | [], [] -> x
  | hd1 :: tl1, hd2 :: tl2 ->
      let r = f hd1 hd2 in
      if r <> x then
        r
      else
        foldl_until2 f r tl1 tl2
  | _ ->
      failwith "unequal list lengths"


let count f l =
  List.fold_left (fun count e ->
    count + if f e then 1 else 0
  ) 0 l


let unordered_append l1 l2 =
  if List.length l1 <= List.length l2 then
    l1 @ l2
  else
    l2 @ l1
