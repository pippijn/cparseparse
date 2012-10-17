module P = Program
module T = Tree
let (|>) = BatPervasives.(|>)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |               This will associate a node with location                | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module TreeMap = Map.Make(struct
  type t = P.location
  let compare = Pervasives.compare
end)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                  Collect types from AST definitions                   | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)
module Collect = struct

  type t = (string * Constr.t TreeMap.t) list

  let rec program program =
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

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |           Print the collected types for debugging purposes            | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let print pp ast_name p =
    program p |> List.iter
        (fun (name, nodes) ->
          ExtFormat.fprintf pp "@[<v>%s\n" name;
          TreeMap.iter (fun (n,t) v ->
            ExtFormat.fprintf pp "@[<h>\t%s::%s->%a@]@;" n t (new Constr.print)#constr v) nodes);
    ExtFormat.fprintf pp "@]@."
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                     Give constructors annotations                     | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Constr = struct
  let rec program p =
    let types = Collect.program p in
    let definition = function
    | P.Map (nm, (s,d), nodes) ->
        let collected = List.assoc s types in
        let constr nm = TreeMap.find nm collected in
        let node (nm, clauses) =
          nm, List.map (fun (l,r) ->
            let rec visit = function
            | T.Tree (_, (tag, tree)) ->
                T.Tree (nm, (tag, List.map visit tree))
            | T.Var (_, nm) -> T.Var ("#undef", nm)
            | T.Const x -> T.Const x
            in
            visit l, visit r) clauses
        in
        P.Map (nm, (s,d), List.map node nodes)
    | P.Ast (a,b) -> P.Ast (a,b)
    in
    List.map definition p
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Give variables annotations                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Vars = struct
  let rec program p =
    let types = Collect.program p in
    let definition = function
    | P.Map (nm, (s,d), nodes) ->
        let collected = List.assoc s types in
        let constr nm = TreeMap.find nm collected in
        let annot (ty, tr) =
          match tr with
        | T.Tree (_, a) -> T.Tree (ty, a)
        | T.Var (_,x) -> T.Var (ty, x)
        | x -> x
        in
        let unify node tree =
          match tree with
          | T.Tree (ty, (x, args')) ->
              let _,args = constr (node,x) in
              T.Tree (ty, (x,List.map annot (List.combine args args')))
          | x -> x
        in
        let node (nm, clauses) =
          nm, List.map (fun (l,r) ->
            let rec visit ty = function
            | T.Tree (x, (tag, tree)) ->
                unify nm (T.Tree (x, (tag, List.map (visit tag) tree)))
            | x -> unify nm x
            in
            visit nm l, r) clauses
        in
        P.Map (nm, (s,d), List.map node nodes)
    | P.Ast (a,b) -> P.Ast (a,b)
    in
    List.map definition p
end

let program p =
  Constr.program p
  |> Vars.program
