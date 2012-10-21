open Sexplib.Conv


module Make(T : Sig.IntegralType) = struct
  type 'a t = 'a array with sexp

  let get array index =
    array.(T.to_int index)

  let set array index value =
    array.(T.to_int index) <- value

  let length = Array.length

  let range array =
    T.range 0 (length array - 1)
end
