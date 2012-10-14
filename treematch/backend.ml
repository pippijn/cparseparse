open Camlp4.PreCast

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost

module OCamlPrinter = Printers.OCaml

module Emit = struct
  let rec program definitions =
    List.map definition definitions
    |> Ast.stSem_of_list

  and definition =
    function Program.Ast (name, nodes) ->
      let first_type :: rest_types = List.map ast_node nodes in
      <:str_item<

        module $uid:name$ = struct
            type t = $first_type$
            and $rest_types |> Ast.tyAnd_of_list$
        end

      >>

  and ast_node (nm, nd) =
    match nd with
    | Program.CustomNode clauses ->
        <:ctyp<

        $lid:nm$ = $List.map ast_clause clauses |> Ast.tyOr_of_list$

        >>
    | Program.NativeNode name ->
        <:ctyp<

          $lid:nm$ = $lid:name$

        >>

  and ast_clause (nm, args) =
    let args = List.map (fun nm -> <:ctyp< $lid:nm$ >>) args in
    <:ctyp< $uid:nm$ of $Ast.tySta_of_list args$>>

end

let output_program file program =
  Emit.program program
  |> OCamlPrinter.print_implem ~output_file:file
