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

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |      Rename all the simple matchers to the one in the production      | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Rename = struct
  let rec program p =
    let types = Collect.program p in
    let is_simple = function
    | T.Tree (_, (_, args)) ->
        List.for_all (function T.Var _ -> true | _ -> false) args
    | _ -> true
    in
    let collector = Hashtbl.create 113 in
    let clause nd (l,r) =
      if is_simple l then
        match l,r with
        | T.Tree (ty,(nm, args)), T.Tree (ty',(nm', args')) ->
            Hashtbl.add collector (nd, nm) nm';
            T.Tree (ty',(nm', args)), T.Tree (ty',(nm', args'))
        | x -> x
      else (l,r)
    in
    let node (nm, clauses) =
      nm, List.map (clause nm) clauses
    in
    let definition = function
    | P.Map (nm, (s,d), nodes) ->
        P.Map (nm, (s,d), List.map node nodes)
    | x -> x
    in
    let replace = function
    | (P.Ast (nm,x)) as a ->
            (* This is ugly! *)
        P.Ast
          (nm,
           List.map (function
           | nm, P.CustomNode x ->
               nm,
             (P.CustomNode
                    (List.map (fun ((nm', args) as c) ->
                      try
                        Hashtbl.find collector (nm,nm'),args
                      with Not_found -> c) x))
           | x -> x) x)
    | x -> x
    in
    let p = List.map definition p in
    List.map replace p
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       Unify types on both sides                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Unify = struct
  let rec program p =
    let types = Collect.program p in
    let definition = function
    | P.Map (nm, (s,d), nodes) ->

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |            Substitute types of variables on left hand side            | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

        let rec substitute s = function
        | T.Tree (ty, (nm, tree)) ->
            T.Tree (ty, (nm, List.map (substitute s) tree))

        | T.Var (_, nm) ->
            T.Var (List.assoc nm s, nm)

        | x -> x
        in
(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |        Collect environment - variables and coresponding types         | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

        let rec collect = function

        | T.Tree (_, (nm, tree)) ->
            List.map collect tree |> List.concat

        | T.Var (ty, nm) ->
            [nm, ty]

        | x -> []
        in

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |          Unify the left and right tree substituing variables          | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

        let rec visit subst = function
            | T.Tree (ty, (nm, tree)), T.Tree (ty', (nm', tree')) ->
                (* FIXME: Add new nodes as apropriate *)
                assert (nm = nm');
                (* FIXME: Remove nodes as apropriate *)
                assert (List.length tree = List.length tree');
                T.Tree (ty', (nm', List.combine tree tree' |> List.map (visit subst)))
            | T.Tree (ty, (nm, tree)), T.Var (_, var) ->
                T.Var (ty, var)

            | T.Var (ty, var), T.Var (_, var') ->
                T.Var (List.assoc var' subst, var')

            | T.Var (ty, var), T.Tree (ty', (nm', tree')) ->
                let subst = (var, ty) :: subst in
                T.Tree (ty', (nm', List.map (substitute subst) tree'))
            | _,x -> x
        in

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Visit single node                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

        let node (nm, clauses) =
          let l,r = List.split clauses in
          let s = List.map collect l |> List.concat in
          let r = List.map (visit s) clauses in
          nm, List.combine l r
        in

        P.Map (nm, (s,d), List.map node nodes)
    | P.Ast (a,b) -> P.Ast (a,b)
    in
    List.map definition p
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                     Generate default productions                      | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Default = struct
  let rec program p =
    let types = Collect.program p in

    let is_simple = function
    | T.Tree (_, (_, args)) ->
        List.for_all (function T.Var _ -> true | _ -> false) args
    | _ -> true
    in

    let collect_simple (l,r) =
      if is_simple l then
        match l with
        | T.Tree (_, (nm,_)) ->
            Some nm
        | _ -> None
      else None
    in

    let node defs (nm, clauses) =
      let to_tree (n, args) =
        Tree.Tree (nm, (n, List.map (fun (ty,nm) -> Tree.Var (ty,nm)) (List.combine args (List.map String.lowercase args)))) in
      let simple = BatList.filter_map collect_simple clauses in
      let simple = List.map (fun x -> nm,x) simple in
      let set = List.fold_right TreeMap.remove simple defs in
      nm, TreeMap.fold (fun (node,name) constr acc ->
        let t = to_tree constr in
        ((t,t)) :: acc) set []
    in
    let definition = function
    | P.Map (nm, (s,d), nodes) ->
        begin try
          let nd = List.assoc s types in
          let generated = (List.map (node nd) nodes) in
          P.Map (nm, (s,d), nodes @ generated)
        with Not_found -> P.Map (nm, (s,d), nodes)
        end
    | x -> x
    in

    List.map definition p

end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                        Type check the program                         | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

(* module Check = struct *)
(*   let rec program p = *)
(*     let types = Collect.program p in *)
(*     let definition = function *)
(*     | P.Map (nm, (s,d), nodes) -> *)

(*     | P.Ast (a,b) -> P.Ast (a,b) *)
(*     in *)
(*     List.map definition p *)
(* end *)

let program p =
  Constr.program p
  |> Vars.program
  |> Rename.program
  |> Unify.program
  |> Default.program