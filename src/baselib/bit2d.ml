module Make(T : Sig.IntegralType) = struct
  open Sexplib.Conv

  type integer = T.t

  type bitset = {
    width  : int;
    height : int;
    bitset : Sig.writable BitSet.Internal.t;
  } with sexp

  type 'm t = bitset

  let sexp_of_t () bs = sexp_of_bitset bs
  let t_of_sexp () sx = bitset_of_sexp sx


  let create width height =
    let width  = T.to_int width  + 1 in
    let height = T.to_int height + 1 in
    { width; height; bitset = BitSet.Internal.create (width * height); }

  let empty = {
    width  = 0;
    height = 0;
    bitset = BitSet.Internal.create 0;
  }


  let bit_position_i { width; height; } x y =
    if x >= width then
      failwith (Printf.sprintf "x position %d exceeds matrix width %d" x width);
    if y >= height then
      failwith (Printf.sprintf "y position %d exceeds matrix height %d" y height);
    let n = y * width + x in
    assert (n < width * height);
    n


  let bit_position matrix x y =
    bit_position_i matrix (T.to_int x) (T.to_int y)


  let set matrix x y =
    let n = bit_position matrix x y in
    BitSet.Internal.add n matrix.bitset


  let is_set_i matrix x y =
    let n = bit_position_i matrix x y in
    BitSet.Internal.mem n matrix.bitset

  let is_set matrix x y =
    let n = bit_position matrix x y in
    BitSet.Internal.mem n matrix.bitset


  let test_and_set matrix x y =
    let previous = is_set matrix x y in
    set matrix x y;
    previous


  let print matrix =
    for y = 0 to matrix.height - 1 do
      print_string "[";
      for x = 0 to matrix.width - 1 do
        if is_set_i matrix x y then
          print_string "1"
        else
          print_string "."
      done;
      print_endline "]"
    done


  let readonly = BatPervasives.identity

end
