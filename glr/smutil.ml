(* smutil.ml *)
(* some random utilities I think should be built in to the language *)

let isEmpty = function
  | _ :: _ -> false
  | _      -> true

let isNone = function
  | None -> true
  | Some _ -> false

let isSome = function
  | None -> false
  | Some _ -> true

let getSome = function
  | None -> failwith "getSome applied to None"
  | Some v -> v

(* true if 'o' is a Some, and it equals (==) 'v' *)
let someEquals (o: 'a option) (v: 'a) : bool =
begin
  match o with
  | None -> false
  | Some(v2) -> v2 == v
end
