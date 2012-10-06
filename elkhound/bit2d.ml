open Batteries_uni


type t = {
  width  : int;
  height : int;
  bitset : BitSet.t;
}


let create width height =
  { width; height; bitset = BitSet.create (width * height); }


let bit_position { width; height; } x y =
  if x >= width then
    failwith (Printf.sprintf "x position %d exceeds matrix width %d" x width);
  if y >= height then
    failwith (Printf.sprintf "y position %d exceeds matrix height %d" y height);
  let n = y * width + x in
  assert (n < width * height);
  n


let set matrix x y =
  let n = bit_position matrix x y in
  BitSet.set matrix.bitset n


let is_set matrix x y =
  let n = bit_position matrix x y in
  BitSet.is_set matrix.bitset n


let test_and_set matrix x y =
  let previous = is_set matrix x y in
  set matrix x y;
  previous


let print matrix =
  for y = 0 to matrix.height - 1 do
    print_string "[";
    for x = 0 to matrix.width - 1 do
      if is_set matrix x y then
        print_string "1"
      else
        print_string "."
    done;
    print_endline "]"
  done


let t_of_sexp s =
  create 0 0

let sexp_of_t v =
  Sexplib.Sexp.List []
