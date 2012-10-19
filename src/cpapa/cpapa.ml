open Ccparse

let () =
  Printexc.record_backtrace true;
  Cmdline.run Parser.main
