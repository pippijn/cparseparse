module Parser = Glr.Frontend.Make
  (Ccparse.CcActions)
  (Ccparse.CcTables)
  (Ccparse.CcPtree)
  (Ccparse.CcPtreeActions)
  (Ccparse.CcTreematch)
  (Ccparse.CcTreematchActions)
  (Ccparse.CcTokens)
  (struct

    open Ccparse

    type lexbuf = Lexing.lexbuf
    type token = CcTokens.t

    let from_channel file = Lexing.from_channel
    let token = Lexer.token

  end)


let main inputs =
  if Options._rt () then
    Sched.(setscheduler 0 FIFO { priority = 20 });

  try
    Parser.parse_files
      ~pp:"gcc -I /usr/include/qt4 -xc++ -E -P "
      ~action:(fun trees ->
        if Glr.FrontendOptions._print () then
          List.iter (function
            | file, None -> ()
            | file, Some tree ->
                Sexplib.Sexp.output_hum stdout
                  (Ccabs.Ast.sexp_of_translation_unit tree);
                print_newline ()
          ) trees
      )
      inputs
  with Glr.Frontend.ExitStatus status ->
    exit status


let () =
  Cmdline.run main
