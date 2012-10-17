type severity =
  | Info
  | Warning
  | Error

exception Diagnostic of severity * string

let info    msg = raise (Diagnostic (Info   , msg))
let warning msg = raise (Diagnostic (Warning, msg))
let error   msg = raise (Diagnostic (Error  , msg))


let string_of_severity = function
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
