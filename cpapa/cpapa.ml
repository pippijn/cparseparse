let () =
  Printexc.record_backtrace true;
  Printexc.print Ccparse.main Options.inputs
