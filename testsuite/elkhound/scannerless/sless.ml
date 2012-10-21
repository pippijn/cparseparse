module Parser = Glr.Easy.Make(SlessActions)(SlessTables)(SlessPtree)(SlessPtreeActions)(SlessTokens)
  (struct
    open SlessTreematch

    let ptree = false
    let typed_ptree = true
  end)


let main =
  List.iter (fun file ->
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in

    Parser.parse SlessLexer.token lexbuf (fun () -> ())
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
