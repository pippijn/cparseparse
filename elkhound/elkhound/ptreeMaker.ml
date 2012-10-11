open AnalysisEnvType
open GrammarType


let merge name = function
  | None -> None
  | Some { params = [l; r] as params } ->
      Some {
        params;
        code = Printf.sprintf "%sPtree.%s.Merge (%s, %s)"
          Config.module_name name l r;
      }
  | _ -> failwith "invalid merge function"


let nonterminal nonterm =
  let semtype =
    if nonterm.nt_index = 1 then
      (* synthesised start symbol *)
      Config.module_name ^ "Ptree.t"
    else
      Config.module_name ^ "Ptree." ^ nonterm.nbase.name ^ ".t"
  in
  { nonterm with
    nbase = { nonterm.nbase with
      semtype = semtype;
      (* most function become trivial *)
      dup = None;
      del = None;
    };
    keep = None;
    (* merge creates a merge node *)
    merge = merge nonterm.nbase.name nonterm.merge;
  }


let symbol = function
  | Nonterminal (tag, nonterm) -> Nonterminal (tag, nonterminal nonterm)
  | term -> term


let nonterms nonterms =
  Array.map nonterminal nonterms


let prods prods =
  let action = Buffer.create 0 in

  let max_nt_index =
    Array.fold_left (fun max_nt_index prod ->
      max max_nt_index prod.left.nt_index
    ) 0 prods
  in

  let counts = Array.make (max_nt_index + 1) 0 in
  Array.iter (fun prod ->
    let i = prod.left.nt_index in
    counts.(i) <- counts.(i) + 1
  ) prods;

  Array.map (function
    (* nonterminal with a single production that is a tagged terminal *)
    | { left = { nt_index }; right = [Terminal (tag, { tbase = { semtype } })] } as prod
      when tag <> "" && counts.(nt_index) = 1 ->
        { prod with action = tag }

    | prod ->
        Buffer.clear action;

        (* production 0 is the synthesised start symbol *)
        let right =
          if prod.prod_index = 0 then (
            Buffer.add_string action "top";
            List.map symbol prod.right
          ) else (
            Buffer.add_string action Config.module_name;
            Buffer.add_string action "Ptree.";
            Buffer.add_string action prod.left.nbase.name;
            Buffer.add_string action ".";

            begin match prod.prod_name with
            | None ->
                Buffer.add_string action "P";
                Buffer.add_string action (string_of_int prod.prod_index);
            | Some name ->
                Buffer.add_string action name
            end;

            let unparameterised, right =
              List.fold_left (fun (first, right) sym ->
                let separator = if first then " (" else ", " in

                match sym with
                | Nonterminal ("", _)
                | Terminal ("", _) ->
                    (* nothing to do for untagged symbols *)
                    first, symbol sym :: right

                | Nonterminal (tag, _)
                | Terminal (tag, _) ->
                    Buffer.add_string action separator;
                    Buffer.add_string action tag;
                    false, symbol sym :: right

              ) (true, []) prod.right
            in

            if not unparameterised then
              Buffer.add_string action ")";

            List.rev right
          )
        in

        { prod with
          left = nonterminal prod.left;
          right = right;
          action = Buffer.contents action;
        }
  ) prods
