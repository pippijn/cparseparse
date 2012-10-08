(* arraystack.ml *)
(* stack of pointers implemented as an array *)


(* grow an array *)
let growArray arr newLen =
  Array.init newLen (fun i ->
    if i < Array.length arr then
      arr.(i)
    else
      Obj.magic ()
  )


type 'a t = {
  (* number of (non-null) elements in the array *)
  mutable len : int;

  (* the array; its length may be greater than 'len', to
   * accomodate adding more elements without resizing the array *)
  mutable arr : 'a array;
}


let length { len } = len

let is_empty { len } = len = 0


(* get topmost element but don't change what is stored *)
let top { len; arr } = arr.(len - 1)


(* get topmost and remove it *)
let pop rep =
  rep.len <- rep.len - 1;
  rep.arr.(rep.len)


(* add a new topmost element *)
let push obj rep =
  if rep.len = Array.length rep.arr then
    (* need to expand the array *)
    rep.arr <- growArray rep.arr (rep.len * 2);

  (* put new element into the array at the end *)
  rep.arr.(rep.len) <- obj;
  rep.len <- rep.len + 1


(* get arbitrary element *)
let nth { arr } i =
  arr.(i)


(* set arbitrary element *)
let set { arr } i v =
  arr.(i) <- v


(* iterate *)
let iter f rep =
  for i = 0 to rep.len - 1 do
    f rep.arr.(i)
  done


(* search and return the element index, or -1 for not found *)
let findIndex f rep =
  let index = ref (-1) in
  begin
    try
      for i = 0 to rep.len - 1 do
        if f rep.arr.(i) then (
          index := i;
          raise Exit
        )
      done
    with Exit -> ()
  end;
  !index


(* search and return the element, or None *)
let find f rep =
  let idx = findIndex f rep in
  if idx < 0 then
    None                 (* not found *)
  else
    Some rep.arr.(idx)   (* found *)
  (* XXX: inlined does not seem to be worth it
  let ret = ref None in
  begin
    try
      for i = 0 to rep.len - 1 do
        if f rep.arr.(i) then (
          ret := Some rep.arr.(i);
          raise Exit
        )
      done
    with Exit -> ()
  end;
  !ret
  *)


(* swap contents with another array stack *)
let swap rep obj =
  let tmpLen = rep.len in
  let tmpArr = rep.arr in

  rep.len <- obj.len;
  rep.arr <- obj.arr;

  obj.len <- tmpLen;
  obj.arr <- tmpArr


(* the stack must be given a dummy value for unused array slots *)
let create () =
  { len = 0; arr = Array.make 16 (Obj.magic ()); }
