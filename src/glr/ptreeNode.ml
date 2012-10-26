(* parse tree node for use with ptreeact module *)


(* a node in a parse tree *)
type t = {            
  (* symbol at this node *)
  symbol : string;          
  
  (* array of children *)
  children : t array;

  (* list of ambiguous alternatives to this node *)
  mutable merged : t option;
}


let make_leaf sym =
  {
    symbol = sym;
    merged = None;
    children = [||];
  }

let make sym child_count child_fun =
  {
    symbol = sym;
    merged = None;
    children = Array.init child_count child_fun;
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


(* add an ambiguous alternative *)
let add_alternative self alt =
  (* insert as 2nd element *)
  alt .merged <- self.merged;
  self.merged <- Some alt


let cyclicSkip self indentation out path =
  if GlrOptions._ptree_cycles () then (
    (* does 'self' appear in 'path'? *)
    let idx = Arraystack.index self path in
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

  indentation + GlrOptions._ptree_indent (), lhs, alts


let print_alt self indentation out expand alts lhs ct node =
  if alts > 1 then (
    indent out (indentation - GlrOptions._ptree_indent ());
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
          innerPrint c (indentation + GlrOptions._ptree_indent ())
        ) node.children;

        ct + 1
      ) 1 (Some self));

      if alts > 1 then (
        (* close up ambiguity display *)
        indent out (indentation - GlrOptions._ptree_indent ());
        Printf.fprintf out "----------- end of ambiguous %s -----------\n" lhs
      );
      
      if GlrOptions._ptree_cycles () then
        (* remove myself from the path *)
        ignore (Arraystack.pop path)
    );
  in

  innerPrint self 0(*indentation*)
