(*----------------------------------------------------------------------------
                              Literals
  --------------------------------------------------------------------------*)

type size = [ `I | `L | `LL | `U | `UL | `ULL ]

type 'a t =
    Float of 'a * string
  | Int of 'a * size * string
  | String of 'a * string
  | Char of 'a * string
