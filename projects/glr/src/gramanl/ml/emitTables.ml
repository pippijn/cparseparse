open Camlp4.PreCast

let (|>) = BatPervasives.(|>)
let ghost = Sloc.ghost "emitTables"


(************************************************
 * :: Parse tables
 ************************************************)

let exSem_of_int_list table =
  let _loc = ghost 15 in
  Array.map (fun value ->
    <:expr<$`int:value$>>
  ) table
  |> Array.to_list
  |> Ast.exSem_of_list


let make_tables tables =
  let open Glr.ParseTablesType in

  let _loc = ghost 26 in
  <:str_item<
    let parseTables = Glr.ParseTablesType.({
      numTerms = $`int:tables.numTerms$;
      numNonterms = $`int:tables.numNonterms$;
      numProds = $`int:tables.numProds$;

      numStates = $`int:tables.numStates$;

      actionCols = $`int:tables.actionCols$;
      actionTable = [| $exSem_of_int_list tables.actionTable$ |];

      gotoCols = $`int:tables.gotoCols$;
      gotoTable = [| $exSem_of_int_list tables.gotoTable$ |];

      prodInfo_rhsLen = [| $exSem_of_int_list tables.prodInfo_rhsLen$ |];
      prodInfo_lhsIndex = [| $exSem_of_int_list tables.prodInfo_lhsIndex$ |];

      stateSymbol = [| $exSem_of_int_list tables.stateSymbol$ |];

      ambigTable = [| $exSem_of_int_list tables.ambigTable$ |];

      nontermOrder = [| $exSem_of_int_list tables.nontermOrder$ |];

      startState = $`int:tables.startState$;
      finalProductionIndex = $`int:tables.finalProductionIndex$;
    })
  >>

let make_ml_tables tables dat =
  Marshal.to_channel dat tables [Marshal.No_sharing];

  let _loc = ghost 58 in
  <:sig_item<
    include Glr.ParseTablesType.S
  >>,
  if Options._use_table_dump () then (
    if Options._inline_table_dump () then (
      let data = Marshal.to_string tables [Marshal.No_sharing] in

      if Options._compress_table_dump () then (
        let len = String.length data in

        let compressed = Zlib.compress data in

        Some <:str_item<
          let parseTables : Glr.ParseTablesType.t =
            Marshal.from_string
              (Zlib.uncompress
                $str:String.escaped compressed$
                $`int:len$) 0
        >>
      ) else (
        Some <:str_item<
          let parseTables : Glr.ParseTablesType.t =
            Marshal.from_string $str:String.escaped data$ 0
        >>
      )
    ) else (
      if Options._compress_table_dump () then
        failwith "zlib compression is only supported for inline table dumps";

      Some <:str_item<
        let parseTables : Glr.ParseTablesType.t =
          input_value (open_in_bin "_build/ccparse/gr/ccTables.dat")
      >>
    )
  ) else (
    if Options._gen_table_text () then
      None
    else
      Some (make_tables tables)
  )
