(* ptreenode.ml *)
(* parse tree node for use with ptreeact module *)
(* based on elkhound/ptreenode *)


(* for storing parse tree counts *)
type tree_count = int


(* a node in a parse tree *)
type t = {            
  (* symbol at this node *)
  symbol : string;          
  
  (* array of children *)
  children : t array;

  (* list of ambiguous alternatives to this node *)
  mutable merged : t option;

  (* number of parse trees this node is root of *)
  mutable count : tree_count;
}


let make_leaf sym =
  {
    symbol = sym;
    merged = None;
    children = [||];
    count = 0;
  }

let make sym child_count child_fun =
  {
    symbol = sym;
    merged = None;
    children = Array.init child_count child_fun;
    count = 0;
  }


let indent out n =
  for i = 0 to n - 1 do
    output_char out ' '
  done


let rec merged_fold f init merged =
  match merged with
  | None -> init
  | Some n -> merged_fold f (f init n) n.merged


(* just the length of the 'merged' list *)
let countMergedList self =
  merged_fold (fun v _ -> v + 1) 0 (Some self)


(* count trees rooted here *)
let rec countTrees self =
  (* memoise *)
  if self.count > 0 then (
    self.count
  ) else (
    (* sum over alternatives, product over children
     * (look at me, functional programming boy)
     * never mind, I am obviously not functional programming boy *)

    (* product over children here *)
    let ct =
      Array.fold_left (fun ct c ->
        ct * countTrees c
      ) 1 self.children
    in

    (* alternatives? *)
    let ct =
      match self.merged with
      | Some merged ->
          (* add them too, recursively *)
          ct + countTrees merged
      | None ->
          ct
    in

    self.count <- ct;

    ct
  )


(* add an ambiguous alternative *)
let addAlternative self alt =
  (* insert as 2nd element *)
  alt.merged <- self.merged;
  self.merged <- Some alt


(* indentation per level *)
let indent_inc = 2

(* turn this on to detect cyclicity; there is a performance penalty *)
let checkForCycles = true


let cyclicSkip self indentation out path =
  if checkForCycles then (
    (* does 'self' appear in 'path'? *)
    let idx = Arraystack.findIndex ((==) self) path in
    if idx >= 0 then (
      (* yes; print a cyclicity reference *)
      indent out indentation;
      Printf.fprintf out "[CYCLIC: refers to %d hops up]\n"
                          (Arraystack.length path - idx + 1);
      true   (* return *)
    ) else (
      (* no; add myself to the path *)
      Arraystack.push self path;
      false
    )
  ) else (
    false
  )


let print_merged self indentation symbol =
  (* this is an ambiguity node *)
  let alts = countMergedList self in

  (* get nonterm from first; should all be same *)
  let lhs =
    try
      (* extract first word *)
      let firstSpace = String.index symbol ' ' in
      String.sub symbol 0 firstSpace
    with
    | Not_found ->
        symbol    (* no spaces, use whole thing *)
  in

  indentation + indent_inc, lhs, alts


let print_alt self indentation out expand alts lhs ct node =
  if alts > 1 then (
    indent out (indentation - indent_inc);
    Printf.fprintf out "------------- ambiguous %s: %d of %d ------------\n"
                        lhs ct alts
  );

  indent out indentation;

  let children = node.children in
  let numChildren = Array.length children in

  Printf.fprintf out "%s" node.symbol;

  if expand then (
    (* symbol is just LHS, write out RHS names after "->" *)
    if numChildren > 0 then (
      Printf.fprintf out " ->";
      Array.iter (fun c ->
        Printf.fprintf out " %s" c.symbol
      ) node.children
    )
  );

  Printf.fprintf out "\n"


let print_tree self out expand =
  (* for detecting cyclicity *)      
  let path = Arraystack.create () in
  
  let rec innerPrint self indentation =
    if not (cyclicSkip self indentation out path) then (
      let indentation, lhs, alts =
        match self.merged with
        | Some _ ->
            print_merged self indentation self.symbol
        | None ->
            indentation, "", 1
      in

      (* iterate over interpretations *)
      ignore (merged_fold (fun ct node ->
        print_alt self indentation out expand alts lhs ct node;

        (* iterate over children and print them *)
        Array.iter (fun c ->
          innerPrint c (indentation + indent_inc)
        ) node.children;

        ct + 1
      ) 1 (Some self));

      if alts > 1 then (
        (* close up ambiguity display *)
        indent out (indentation - indent_inc);
        Printf.fprintf out "----------- end of ambiguous %s -----------\n" lhs
      );
      
      if checkForCycles then
        (* remove myself from the path *)
        ignore (Arraystack.pop path)
    );
  in

  innerPrint self 0(*indentation*)
