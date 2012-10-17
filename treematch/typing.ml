module P = Program
let (|>) = BatPervasives.(|>)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |               This will associate a node with location                | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module TreeMap = Map.Make(struct
  type t = P.location
  let compare = Pervasives.compare
end)

let rec nodes program =
  BatList.filter_map definition program
  |> List.map
      (fun (name,lst) ->
        name,
        List.fold_right
          (fun (k,v) set -> TreeMap.add k v set)
          lst TreeMap.empty
      )
and definition = function
| P.Ast (name, nodes) ->
    let nodes = BatList.filter_map
      (function
      | nm, P.CustomNode l -> Some (nm,l)
      | _ -> None) nodes in
    let nodes =
      List.map
        (fun (nm, l) ->
          List.map (fun constr -> (nm, Constr.name constr), constr) l)
        nodes |> List.concat
    in
    Some (name, nodes)
| _ -> None

let print pp ast_name program =
  nodes program |> List.iter
      (fun (name, nodes) ->
        ExtFormat.fprintf pp "@[<v>%s\n" name;
        TreeMap.iter (fun (n,t) v ->
          ExtFormat.fprintf pp "@[<h>\t%s::%s->%a@]@;" n t (new Constr.print)#constr v) nodes);
  ExtFormat.fprintf pp "@]@."
