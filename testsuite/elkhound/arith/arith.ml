module Parser = Glr.Easy.Make(ArithActions)(ArithTables)(ArithPtree)(ArithPtreeActions)(ArithTokens)
  (struct
    let ptree = false
    let typed_ptree = false
  end)


let main =
  List.iter (fun file ->
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in

    Parser.parse ArithLexer.token lexbuf (Printf.printf "result: %d\n")
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main