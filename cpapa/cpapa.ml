open Ccparse

let () =
  Printexc.record_backtrace true;
  Printexc.print Cmdline.run Parser.main
