open Sexplib.Conv

type ('a, 'mutability, 'integer) repr = 'a array with sexp

let append = Array.append

module Make(T : Sig.IntegralType) = struct
  type integer = T.t
  type ('a, 'm) t = ('a, 'm, T.t) repr


  (* private functions *)
  let mapi_fun f i e = f (T.of_int i) e

  (* public functions *)
  let t_of_sexp sx () = repr_of_sexp sx () ()
  let sexp_of_t ar () = sexp_of_repr ar () ()

  let readonly = BatPervasives.identity

  let to_array = BatPervasives.identity
  let to_list = Array.to_list

  let get array index =
    array.(T.to_int index)

  let set array index value =
    array.(T.to_int index) <- value

  let length = Array.length

  let range array =
    T.range 0 (length array - 1)

  let empty = [||]
  let make = Array.make
  let init l f = Array.init l (fun i -> f (T.of_int i))

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let foldl_untili f = ExtArray.foldl_untili (mapi_fun f)
  let foldl_until = ExtArray.foldl_until
  let iteri f = Array.iteri (mapi_fun f)
  let iter = Array.iter
  let map = Array.map
  let mapi f = Array.mapi (mapi_fun f)
  let find = BatArray.find
  let exists = BatArray.exists

  let sum = ExtArray.sum
  let count = ExtArray.count
end
