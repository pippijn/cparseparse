open Batteries_uni
open GrammarType
open AnalysisEnvType
open LrItem
open ItemSet


let print_dotted_production dprod =
  print_string "  ->";
  List.iteri (fun position rhs ->
    print_string " ";
    let after_dot =
      match dprod.after_dot with
      | None -> false
      | Some sym -> rhs == sym
    in
    if position = dprod.dot then
      print_string ".[";
    PrintGrammar.print_symbol rhs;
    if after_dot then
      print_string "]";
  ) dprod.prod.right;

  if dprod.dot = List.length dprod.prod.right then
    (* dot is at end *)
    print_string " .";

  print_newline ()


let print_dotted_productions env =
  Array.iteri (fun prod_index dprods ->
    print_endline env.indexed_prods.(prod_index).left.nbase.name;
    Array.iter print_dotted_production dprods
  ) env.dotted_prods


let print_lr_item env item =
  ()


let print_item_set env item_set =
  Printf.printf "ItemSet %d:\n" (int_of_state_id item_set.state_id);
  Printf.printf "  kernel_items :\n";
  List.iter (print_lr_item env) item_set.kernel_items;
  Printf.printf "  nonkernel_items :\n";
  List.iter (print_lr_item env) item_set.nonkernel_items;
