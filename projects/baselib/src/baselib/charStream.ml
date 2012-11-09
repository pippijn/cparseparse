type t = {
  data : string;
  mutable pos : int;
}


let of_string data = { data; pos = 0 }


let next stream =
  let c = stream.data.[stream.pos] in
  stream.pos <- stream.pos + 1;
  c


let take n stream =
  let s = String.sub stream.data stream.pos n in
  stream.pos <- stream.pos + n;
  s


let drop n stream =
  stream.pos <- stream.pos + n


let rec foldl f x stream =
  match f x (next stream) with
  | x, true  ->
      x
  | x, false ->
      foldl f x stream


let is_empty stream =
  stream.pos == String.length stream.data


let rec parse f a stream =
  if is_empty stream then
    a
  else
    parse f (f a stream) stream


let to_string stream =
  String.sub stream.data stream.pos (String.length stream.data - stream.pos)
