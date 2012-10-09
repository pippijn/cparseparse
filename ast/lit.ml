(*----------------------------------------------------------------------------
                              Literals
  --------------------------------------------------------------------------*)

type 'a t =
    Float of 'a * string
  | Int of 'a * size * string
  | String of 'a * string
  | Char of 'a * string
and size = [| `I | `L | `LL | `U | `UL | `ULL |]
