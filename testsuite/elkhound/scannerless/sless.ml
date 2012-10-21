module Parser = Glr.Easy.Make
  (SlessActions)(SlessTables)
  (SlessPtree)(SlessPtreeActions)
  (SlessTreematch)(SlessTreematchActions)
  (SlessTokens)
  (struct
    let ptree = false
    let typed_ptree = true
    let treematch = false
    let user = false
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
