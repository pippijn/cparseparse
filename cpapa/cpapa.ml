let () =
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  Printexc.record_backtrace true;
  Printexc.print Ccparse.main Options.inputs
