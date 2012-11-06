module type S = sig
  val black   : string
  val red     : string
  val green   : string
  val yellow  : string
  val blue    : string
  val pink    : string
  val cyan    : string
  val white   : string

  val bblack  : string
  val bred    : string
  val bgreen  : string
  val byellow : string
  val bblue   : string
  val bpink   : string
  val bcyan   : string
  val bwhite  : string

  val reset   : string
end


module ANSI : S = struct
  let black   = "[0;30m"
  let red     = "[0;31m"
  let green   = "[0;32m"
  let yellow  = "[0;33m"
  let blue    = "[0;34m"
  let pink    = "[0;35m"
  let cyan    = "[0;36m"
  let white   = "[0;37m"

  let bblack  = "[1;30m"
  let bred    = "[1;31m"
  let bgreen  = "[1;32m"
  let byellow = "[1;33m"
  let bblue   = "[1;34m"
  let bpink   = "[1;35m"
  let bcyan   = "[1;36m"
  let bwhite  = "[1;37m"

  let reset   = "[0m"
end


module None : S = struct
  let black   = ""
  let red     = ""
  let green   = ""
  let yellow  = ""
  let blue    = ""
  let pink    = ""
  let cyan    = ""
  let white   = ""

  let bblack  = ""
  let bred    = ""
  let bgreen  = ""
  let byellow = ""
  let bblue   = ""
  let bpink   = ""
  let bcyan   = ""
  let bwhite  = ""

  let reset   = ""
end


module Make(Term : S) = struct
  open Term

  let black   s = black   ^ s ^ reset
  let red     s = red     ^ s ^ reset
  let green   s = green   ^ s ^ reset
  let yellow  s = yellow  ^ s ^ reset
  let blue    s = blue    ^ s ^ reset
  let pink    s = pink    ^ s ^ reset
  let cyan    s = cyan    ^ s ^ reset
  let white   s = white   ^ s ^ reset
                               
  let bblack  s = bblack  ^ s ^ reset
  let bred    s = bred    ^ s ^ reset
  let bgreen  s = bgreen  ^ s ^ reset
  let byellow s = byellow ^ s ^ reset
  let bblue   s = bblue   ^ s ^ reset
  let bpink   s = bpink   ^ s ^ reset
  let bcyan   s = bcyan   ^ s ^ reset
  let bwhite  s = bwhite  ^ s ^ reset
end
