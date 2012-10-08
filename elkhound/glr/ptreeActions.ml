(* ptreeact.ml *)
(* given actions for a grammar, wrap them with actions that
 * just build a parse tree (forsest) *)
(* based on elkhound/ptreeact *)


open Lexerint
open ParseTables
open PtreeNode


let inject : PtreeNode.t -> SemanticValue.t = Obj.magic
let project : SemanticValue.t -> PtreeNode.t = Obj.magic



(* ------------------------ parseTreeLexer ------------------------- *)
(* wrap the lexer with one yielding parse tree leaf nodes *)
let getToken actions underlying lex =
  let open UserActions in
  (* get next token *)
  let lex = underlying lex in
  (* my sval is a tree leaf *)
  lex.tokSval <- inject (makePTreeNode (actions.terminalName lex.tokType));
  lex


(* ----------------------- parseTreeActions ------------------------ *)
let makeParseTreeActions underlying tables : PtreeNode.t UserActions.t =
  UserActions.({
    (* action to perform upon performing a reduction *)
    reductionAction = (
      fun prodId svals ->
        (* production info *)
        let rhsLen = getProdInfo_rhsLen tables prodId in
        let lhsIndex = getProdInfo_lhsIndex tables prodId in
        
        (* make a tree node, initially with no children *)
        let ret = makePTreeNode (underlying.nonterminalName lhsIndex) in

        (* add the children *)
        setNumChildren ret rhsLen;
        for i = 0 to rhsLen - 1 do
          let child = project svals.(i) in
          setChild ret i child;
        done;
        
        inject ret
    );
      
    (* duplicate a semantic value: trivial *)
    duplicateTerminalValue = (fun _ sval -> sval);
    duplicateNontermValue  = (fun _ sval -> sval);

    (* deallocate an sval that didn't get used: trivial *)
    deallocateTerminalValue = (fun _ _ -> ());
    deallocateNontermValue  = (fun _ _ -> ());

    (* merge svals for alternative derivations of the same nonterminal *)
    mergeAlternativeParses = (
      fun ntIndex left right ->
        let l = project left in
        let r = project right in
        
        addAlternative l r;
        inject l
    );

    (* choose whether to keep or drop a reduced value: trivial *)
    keepNontermValue = (fun _ _ -> true);

    terminalDescription    = (fun id _ -> (underlying.terminalName    id));
    nonterminalDescription = (fun id _ -> (underlying.nonterminalName id));
    terminalName           = (fun id   -> (underlying.terminalName    id));
    nonterminalName        = (fun id   -> (underlying.nonterminalName id));
  })
