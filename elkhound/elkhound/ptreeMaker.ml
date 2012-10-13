open AnalysisEnvType
open GrammarType
open Camlp4.PreCast

let (|>) = BatPervasives.(|>)

let _loc = Loc.ghost


let merge name = function
  | None -> None
  | Some { params = [l; r] as params } ->
      Some {
        params;
        code = <:expr<$uid:Config.module_name ^ "Ptree"$.$uid:name$.Merge ($lid:l$, $lid:r$)>>
      }
  | _ -> failwith "invalid merge function"


let nonterminal nonterm =
  let semtype =
    if nonterm.nt_index = 1 then
      (* synthesised start symbol *)
      <:ctyp<$uid:Config.module_name ^ "Ptree"$.t>>
    else
      <:ctyp<$uid:Config.module_name ^ "Ptree"$.$uid:nonterm.nbase.name$.t>>
  in
  { nonterm with
    nbase = { nonterm.nbase with
      semtype = Some semtype;
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


let counts fold_left iter prods =
  let max_nt_index =
    fold_left (fun max_nt_index prod ->
      max max_nt_index prod.left.nt_index
    ) 0 prods
  in

  let counts = Array.make (max_nt_index + 1) 0 in
  iter (fun prod ->
    let i = prod.left.nt_index in
    counts.(i) <- counts.(i) + 1
  ) prods;

  counts

let counts_of_array = counts Array.fold_left Array.iter
let counts_of_list  = counts List.fold_left List.iter


(* XXX: if this function changes its output, EmitCode.production_types probably
 * also needs to change *)
let prods prods =
  let counts = counts_of_array prods in

  Array.map (function
    (* nonterminal with a single production that is a tagged terminal *)
    | { left = { nt_index }; right = [Terminal (tag, _)] } as prod
          when tag <> "" && counts.(nt_index) = 1 ->
        { prod with action = Some <:expr<$lid:tag$>> }

    | prod ->
        let left = nonterminal prod.left in
        let right = List.map symbol prod.right in

        let action =
          (* production 0 is the synthesised start symbol *)
          if prod.prod_index = 0 then (
            <:expr<top>>
          ) else (
            let prod_name =
              match prod.prod_name with
              | None      -> "P" ^ string_of_int prod.prod_index
              | Some name -> assert (name <> ""); name
            in

            let prod_variant =
              <:expr<$uid:Config.module_name ^ "Ptree"$.$uid:left.nbase.name$.$uid:prod_name$>>
            in

            List.fold_left (fun ctor sym ->
              match sym with
              | Nonterminal ("", _)
              | Terminal ("", _) ->
                  (* nothing to do for untagged symbols *)
                  ctor

              | Nonterminal (tag, _)
              | Terminal (tag, _) ->
                  <:expr<$ctor$ $lid:tag$>>
            ) prod_variant prod.right
          )
        in

        { prod with left; right; action = Some action; }
  ) prods
