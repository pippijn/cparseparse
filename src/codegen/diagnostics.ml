type severity =
  | Info
  | Warning
  | Error

let string_of_severity = function
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"

exception Exit

let diagnostic severity at msg =
  let _loc, _ = Sloc._loc at in
  Printf.printf "%s:\n%s: %s\n"
    (Camlp4.PreCast.Loc.to_string _loc)
    (string_of_severity severity)
    msg

let info at msg = diagnostic Info at msg
let warning at msg = diagnostic Warning at msg
let error at msg =
  diagnostic Error at msg;
  raise Exit
