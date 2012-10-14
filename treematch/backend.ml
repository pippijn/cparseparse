open Camlp4.PreCast

let _loc = Loc.ghost

module OCamlPrinter = Printers.OCaml

let (|>) x f = f x

module Emit = struct
  let rec program definitions =
    List.map definition definitions
    |> Ast.stSem_of_list
  and definition _ = <:str_item< let () = ()>>
end

let output_program file program =
  Emit.program program
  |> OCamlPrinter.print_implem ~output_file:file
