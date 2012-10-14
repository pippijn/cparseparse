open Camlp4.PreCast
open GrammarType
open CodegenHelpers

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost


(************************************************
 * :: Parse tables
 ************************************************)

let exSem_of_int_list table =
  Array.map (fun value ->
    <:expr<$int:string_of_int value$>>
  ) table
  |> Array.to_list
  |> Ast.exSem_of_list


let print_tables tables =
  let open ParseTablesType in

  <:str_item<
    let parseTables = ParseTablesType.({
      numTerms = $int:string_of_int tables.numTerms$;
      numNonterms = $int:string_of_int tables.numNonterms$;
      numProds = $int:string_of_int tables.numProds$;

      numStates = $int:string_of_int tables.numStates$;

      actionCols = $int:string_of_int tables.actionCols$;
      actionTable = [| $exSem_of_int_list tables.actionTable$ |];

      gotoCols = $int:string_of_int tables.gotoCols$;
      gotoTable = [| $exSem_of_int_list tables.gotoTable$ |];

      prodInfo_rhsLen = [| $exSem_of_int_list tables.prodInfo_rhsLen$ |];
      prodInfo_lhsIndex = [| $exSem_of_int_list tables.prodInfo_lhsIndex$ |];

      stateSymbol = [| $exSem_of_int_list tables.stateSymbol$ |];

      ambigTable = [| $exSem_of_int_list tables.ambigTable$ |];

      nontermOrder = [| $exSem_of_int_list tables.nontermOrder$ |];

      startState = $int:string_of_int tables.startState$;
      finalProductionIndex = $int:string_of_int tables.finalProductionIndex$;
    })
  >>

let make_ml_tables dat tables =
  Marshal.to_channel dat tables [Marshal.No_sharing];

  <:sig_item<
    val parseTables : ParseTablesType.t
  >>,
  if Config.use_table_dump then (
    Some <:str_item<
      let parseTables : ParseTablesType.t =
        input_value (open_in_bin "_build/ccparse/gr/ccTables.dat")
    >>
  ) else (
    if true then
      None
    else
      Some (print_tables tables)
  )


