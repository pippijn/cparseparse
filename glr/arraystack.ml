(* arraystack.ml *)
(* stack of pointers implemented as an array *)


(* grow an array *)
let growArray arr newLen null =
  let newArr = Array.make newLen null in

  (* copy *)
  Array.blit
    arr                   (* source array *)
    0                     (* source start position *)
    newArr                (* dest array *)
    0                     (* dest start position *)
    (Array.length arr);   (* number of elements to copy *)

  (* return new array *)
  newArr


(* ensure the array has at least the given index, growing its size
 * if necessary (by doubling) *)
let ensureIndexDoubler arr idx null =
  while Array.length !arr < idx + 1 do
    arr := growArray !arr (Array.length !arr * 2) null;
  done


type 'a t = {
  null : 'a;

  (* number of (non-null) elements in the array *)
  mutable len : int;

  (* the array; its length may be greater than 'poolLength', to
   * accomodate adding more elements without resizing the array *)
  mutable arr : 'a array;
}


let length { len } = len

let isEmpty { len } = len = 0
let isNotEmpty { len } = len > 0


(* get topmost element but don't change what is stored *)
let top { len; arr } = arr.(len - 1)


(* get topmost and remove it *)
let pop rep =
  rep.len <- rep.len - 1;
  rep.arr.(rep.len)


(* add a new topmost element *)
let push rep obj =
  if rep.len = Array.length rep.arr then
    (* need to expand the array *)
    rep.arr <- growArray rep.arr (rep.len * 2) rep.null;

  (* put new element into the array at the end *)
  rep.arr.(rep.len) <- obj;
  rep.len <- rep.len + 1


(* get arbitrary element *)
let elt { arr } i =
  arr.(i)


(* set arbitrary element *)
let setElt { arr } i v =
  arr.(i) <- v


(* iterate *)
let iter rep f =
  for i = 0 to rep.len - 1 do
    f rep.arr.(i)
  done


(* search and return the element index, or -1 for not found *)
let findIndex rep f =
  (* ug.. must use tail recursion just so I can break early... *)
  let rec loop i =
    if i > rep.len - 1 then
      -1                 (* not found *)
    else if f rep.arr.(i) then
      i                  (* found *)
    else
      loop (i + 1)       (* keep looking *)
  in

  loop 0


(* search and return the element, or None *)
let findOption rep f =
  let idx = findIndex rep f in
  if idx < 0 then
    None                 (* not found *)
  else
    Some rep.arr.(idx)   (* found *)


(* search *)
let contains rep f =
  findIndex rep f >= 0


(* swap contents with another array stack *)
let swapWith rep obj =
  let tmpLen = rep.len in
  let tmpArr = rep.arr in

  rep.len <- obj.len;
  rep.arr <- obj.arr;

  obj.len <- tmpLen;
  obj.arr <- tmpArr


(* the stack must be given a dummy value for unused array slots *)
let make null =
  { null; len = 0; arr = Array.make 16 null; }


(* EOF *)
