(* glr.ml *)
(* GLR parser *)
(* based on elkhound/glr.h and elkhound/glr.cc *)

open Useract         (* tSemanticValue *)
open Parsetables     (* action/goto/etc. *)
open Smutil          (* getSome, etc. *)


(* Relative to C++ implementation, what is not done:
 *   - Token reclassification
 *   - Table compression
 *   - Heavy testing of the mini-LR core
 *)


(* when true, print parse actions *)
let traceParse  = false

(* when true, keep some statistics useful for performance evaluation *)
let accounting  = true

(* when true, we call the user's keep() functions *)
let use_keep    = true

(* when true, use the mini LR core *)
let use_mini_lr = true


(* NOTE: in some cases, more detailed comments can be found in
 * elkhound/glr.h, as these data structures mirror the ones
 * defined there *)


(* identifier for a symbol *)
type symbol_id = int


(* link from one stack node to another *)
type sibling_link = {
  (* stack node we're pointing at; == cNULL_STACK_NODE if none *)
  mutable sib : stack_node;

  (* semantic value on this link *)
  mutable sval : tSemanticValue;

  (* TODO: source location *)

  (* possible TODO: yield count *)
}

(* node in the GLR graph-structured stack; all fields are
 * mutable because these are stored in a pool for explicit re-use *)
and stack_node = {
  (* for access to parser context in a few unusual situations *)
  glr : glr;

  (* LR parser state when this node is at the top *)
  mutable state : state_id;

  (* pointers to adjacent (to the left) stack nodes *)
  (* possible TODO: put links into a pool so I can deallocate them *)
  mutable leftSiblings : sibling_link list;

  (* logically first sibling in the sibling list; separated out
   * from 'leftSiblings' for performance reasons *)
  mutable firstSib : sibling_link;

  (* number of sibling links pointing at this node, plus the
   * number of worklists this node appears in *)
  mutable referenceCount : int;

  (* number of links we can follow to the left before hitting a node
   * that has more than one sibling *)
  mutable determinDepth : int;

  (* position of token that was active when this node was created
   * (or pulled from pool); used in yield-then-merge calculations *)
  mutable column : int;
}

(* this is a path that has been queued for reduction;
 * all fields mutable to support pooling *)
and path = {
  (* array of sibling links, i.e. the path; 0th element is
   * leftmost link *)
  sibLinks : sibling_link array ref;

  (* corresponding array of symbol ids to interpret svals *)
  symbols : symbol_id array ref;

  (* rightmost state's id *)
  mutable startStateId : state_id;

  (* production we're going to reduce with *)
  mutable prodIndex : int;

  (* column from leftmost stack node *)
  mutable startColumn : int;

  (* the leftmost stack node itself *)
  mutable leftEdgeNode : stack_node;

  (* next path in dequeueing order *)
  mutable next : path option;
}

(* priority queue of reduction paths *)
and reduction_path_queue = {
  (* head of the list, first to dequeue *)
  mutable top : path option;

  (* pool of path objects *)
  pathPool : path Objpool.t;

  (* need our own copy of the tables pointer *)
  rpqTables : tParseTables;      (* name can't collide with glr.tables.. ! *)
}


(* GLR parser object *)
(* some mutable fields are for hack in 'makeGLR' *)
and glr = {
  (* user-specified actions *)
  userAct : tUserActions;

  (* parse tables from the grammar *)
  tables : tParseTables;

  (* treat this as a local variable of rwlProcessWorklist, included
   * here just to avoid unnecessary repeated allocation *)
  toPass : tSemanticValue array;

  (* reduction queue and pool *)
  pathQueue : reduction_path_queue;

  (* set of topmost parser nodes *)
  topmostParsers : stack_node Arraystack.t;

  (* swapped with 'topmostParsers' periodically, for performance reasons *)
  prevTopmost : stack_node Arraystack.t;

  (* node allocation pool; shared with glrParse *)
  mutable stackNodePool : stack_node Objpool.t;

  (* when true, print some diagnosis of failed parses *)
  noisyFailedParse : bool;

  (* current token number *)
  mutable globalNodeColumn : int;

  (* parser action statistics *)
  mutable detShift : int;
  mutable detReduce : int;
  mutable nondetShift : int;
  mutable nondetReduce : int;
}

(* Mini-LR parser object *)
type tLR = {
  lrToPass : tSemanticValue array;

  mutable lrDetShift : int;
  mutable lrDetReduce : int;
}



(* what follows is based on elkhound/glr.cc *)

(* maximum RHS length for mini-lr core *)
let cMAX_RHSLEN = 8
let cINITIAL_RHSLEN_SIZE = 8


(* ------------------ accounting statistics ----------------- *)
let numStackNodesAllocd = ref 0
let maxStackNodesAllocd = ref 0


(* ----------------- front ends to user code --------------- *)
let symbolDescription sym user sval =
  if symIsTerm sym then
    user.terminalDescription (symAsTerm sym) sval
  else
    user.nonterminalDescription (symAsNonterm sym) sval


let duplicateSemanticValue glr sym sval =
  assert (sym <> 0);

  (* the C++ implementation checks for NULL sval, but I don't think
   * that can be here in the ML version, and I'm not convinced the
   * check would even be safe *)

  if symIsTerm sym then
    glr.userAct.duplicateTerminalValue (symAsTerm sym) sval
  else
    glr.userAct.duplicateNontermValue (symAsNonterm sym) sval


let deallocateSemanticValue sym user sval =
  assert (sym <> 0);

  if symIsTerm sym then
    user.deallocateTerminalValue (symAsTerm sym) sval
  else
    user.deallocateNontermValue (symAsNonterm sym) sval


(* --------------------- SiblingLink ----------------------- *)
(* NULL sibling link *)
let cNULL_SIBLING_LINK = {
  sib = Obj.magic [];
  sval = cNULL_SVAL;
}

let makeSiblingLink sib sval = { sib; sval; }


(* --------------------- StackNode -------------------------- *)
(* NULL stack node *)
let cNULL_STACK_NODE = {
  state          = cSTATE_INVALID;
  leftSiblings   = [];
  firstSib       = (Obj.magic []);
  referenceCount = 0;
  determinDepth  = 0;
  glr            = (Obj.magic []);
  column         = 0;
}


let emptyStackNode glr = {
  state          = cSTATE_INVALID;
  leftSiblings   = [];
  firstSib       = makeSiblingLink cNULL_STACK_NODE cNULL_SVAL;
  referenceCount = 0;
  determinDepth  = 0;
  glr            = glr;
  column         = 0;
}


let getNodeSymbol node =
  node.glr.tables.stateSymbol.(node.state)


let incRefCt node =
  node.referenceCount <- node.referenceCount + 1


let rec decRefCt node =
  assert (node.referenceCount > 0);

  node.referenceCount <- node.referenceCount - 1;

  (*(Printf.printf "decrementing node %d to %d\n" node.state node.referenceCount);*)
  (*(flush stdout);*)

  if node.referenceCount = 0 then (
    deinitStackNode node;
    Objpool.dealloc node.glr.stackNodePool node
  )


and deinitStackNode node =
  deallocSemanticValues node;

  (* this is implicit in the C++ implementation because firstSib.sib
   * is an RCPtr in C++ *)
  if node.firstSib.sib != cNULL_STACK_NODE then
    decRefCt node.firstSib.sib;

  node.firstSib.sib <- cNULL_STACK_NODE;

  if accounting then (
    decr numStackNodesAllocd;
    (*
      (Printf.printf "(...) deinit stack node: num=%d max=%d\n"
                     !numStackNodesAllocd
                     !maxStackNodesAllocd);
      (flush stdout);
    *)
  )


and deallocSemanticValues node =
  if node.firstSib.sib != cNULL_STACK_NODE then
    deallocateSemanticValue (getNodeSymbol node) node.glr.userAct node.firstSib.sval;

  List.iter (fun s ->
    deallocateSemanticValue (getNodeSymbol node) node.glr.userAct s.sval;

    (* this is implicit in the C++ version, due to Owner<> *)
    decRefCt s.sib
  ) node.leftSiblings;

  node.leftSiblings <- []


let initStackNode node st =
  node.state <- st;
  assert (isEmpty node.leftSiblings);
  assert (node.firstSib.sib == cNULL_STACK_NODE);
  node.referenceCount <- 0;
  node.determinDepth <- 1;

  if accounting then (
    incr numStackNodesAllocd;
    if !numStackNodesAllocd > !maxStackNodesAllocd then
      maxStackNodesAllocd := !numStackNodesAllocd;
    (*
      (Printf.printf "(!!!) init stack node: num=%d max=%d\n"
                     !numStackNodesAllocd
                     !maxStackNodesAllocd);
      (flush stdout);
    *)
  )


let hasZeroSiblings node =
  node.firstSib.sib == cNULL_STACK_NODE


let hasOneSibling node =
  node.firstSib.sib != cNULL_STACK_NODE && isEmpty node.leftSiblings


let hasMultipleSiblings node =
  not (isEmpty node.leftSiblings)


let addFirstSiblingLink_noRefCt node leftSib sval =
  assert (hasZeroSiblings node);

  node.determinDepth <- leftSib.determinDepth + 1;

  assert (node.firstSib.sib == cNULL_STACK_NODE);
  node.firstSib.sib <- leftSib;     (* update w/o refct *)

  node.firstSib.sval <- sval


let addAdditionalSiblingLink node leftSib sval =
  (* now there is a second outgoing pointer *)
  node.determinDepth <- 0;

  (* this was implicit in the C++ verison *)
  incRefCt leftSib;

  let link = makeSiblingLink leftSib sval in
  node.leftSiblings <- link :: node.leftSiblings;

  link


let addSiblingLink node leftSib sval =
  if node.firstSib.sib == cNULL_STACK_NODE then (
    addFirstSiblingLink_noRefCt node leftSib sval;

    (* manually inc refct *)
    incRefCt leftSib;

    (* pointer to firstSib.. *)
    node.firstSib
  ) else (
    addAdditionalSiblingLink node leftSib sval
  )


let getUniqueLink node =
  assert (hasOneSibling node);
  node.firstSib


let getLinkTo node another =
  (* first? *)
  if node.firstSib.sib == another then (
    Some node.firstSib
  ) else (
    (* rest? *)
    try
      let link = List.find (fun candidate -> candidate.sib == another) node.leftSiblings in
      Some link
    with Not_found ->
      None
  )


(* printAllocStats goes here *)

let computeDeterminDepth node =
  if hasZeroSiblings node then (
    1
  ) else if hasOneSibling node then (
    (* sibling's plus one *)
    node.firstSib.sib.determinDepth + 1
  ) else (
    assert (hasMultipleSiblings node);
    0
  )


let checkLocalInvariants node =
  computeDeterminDepth node = node.determinDepth


(* ----------------------- stack node list ops ---------------------- *)
let decParserList lst =
  Arraystack.iter lst decRefCt


let incParserList lst =
  Arraystack.iter lst incRefCt


let parserListContains lst n =
  Arraystack.contains ((==) n) lst


(* ----------------------------- GLR --------------------------------- *)
let makePath () = {
  startStateId = cSTATE_INVALID;
  prodIndex    = -1;
  startColumn  = -1;
  leftEdgeNode = cNULL_STACK_NODE;
  sibLinks     = ref (Array.make cINITIAL_RHSLEN_SIZE cNULL_SIBLING_LINK);
  symbols      = ref (Array.make cINITIAL_RHSLEN_SIZE 0);
  next         = None;
}


let makeReductionPathQueue tablesIn = {
  top = None;
  pathPool = Objpool.make makePath;
  rpqTables = tablesIn;
}


let makeGLR tablesIn actions =
  let glr = {
    userAct             = actions;
    tables              = tablesIn;
    toPass              = Array.make cMAX_RHSLEN cNULL_SVAL;
    pathQueue           = makeReductionPathQueue tablesIn;
    topmostParsers      = Arraystack.make cNULL_STACK_NODE;
    prevTopmost         = Arraystack.make cNULL_STACK_NODE;
    stackNodePool       = Objpool.null ();
    noisyFailedParse    = true;
    globalNodeColumn    = 0;
    detShift            = 0;
    detReduce           = 0;
    nondetShift         = 0;
    nondetReduce        = 0
  } in

  (* finish nasty hack: I've got a bootstrapping problem where I can't
   * make the pool before the GLR, nor the other way around; so I fell back
   * on the trusted_cast to get it off the ground, and now I have to
   * fix it
   *
   * The main problem here is that because I'm doing explicit deallocation,
   * ML gets to see the objects while they're in limbo.  I *could* mark
   * some fields 'option' but that would not properly reflect the design.
   * So I prefer this local (if gross) hack to something that pollutes the
   * design itself.
   *
   * In fact, I *did* use the 'option' approach for sibling_link.sib,
   * and it is indeed a pain.
   * UPDATE: Switched to using Obj.magic there too, for performance.
   *)

  glr.stackNodePool <- Objpool.make (fun () -> emptyStackNode glr);

  if use_mini_lr then
    (* check that none of the productions exceed cMAX_RHSLEN *)
    for i = 0 to glr.tables.numProds - 1 do
      let len = getProdInfo_rhsLen glr.tables i in

      if len > cMAX_RHSLEN then (
        (* I miss token concatenation...*)
        Printf.printf "Production %d contains %d right-hand side symbols,\n" i len;
        Printf.printf "but the GLR core has been compiled with a limit of %d.\n" cMAX_RHSLEN;
        Printf.printf "Please adjust cMAX_RHSLEN and recompile the GLR core.\n";
        failwith "cannot continue";
      )
    done;

  glr


(* printConfig goes here *)

let grabTopSval glr node =
  let sib = getUniqueLink node in
  let ret = sib.sval in
  sib.sval <- duplicateSemanticValue glr (getNodeSymbol node) sib.sval;

  (* TRSACTION("dup'd " << ret << " for top sval, yielded " << sib->sval); *)

  ret


(* macro in C++ version *)
let makeStackNode glr state =
  let dest = Objpool.alloc glr.stackNodePool in
  initStackNode dest state;
  dest.column <- glr.globalNodeColumn;
  dest


let addTopmostParser glr parsr =
  assert (checkLocalInvariants parsr);

  Arraystack.push glr.topmostParsers parsr;
  incRefCt parsr


(* stackTraceString *)


let initPath path ssi pi rhsLen =
  path.startStateId <- ssi;
  path.prodIndex <- pi;

  (* just use the 0th elements as the dummy 'null' value *)
  Arraystack.ensureIndexDoubler path.sibLinks rhsLen !(path.sibLinks).(0);
  Arraystack.ensureIndexDoubler path.symbols  rhsLen !(path.symbols ).(0)


let newPath queue ssi pi rhsLen =
  let p = Objpool.alloc queue.pathPool in
  initPath p ssi pi rhsLen;
  p


let goesBefore queue p1 p2 =
  if p1.startColumn > p2.startColumn then (
    (* 'p1' spans fewer tokens, so it goes first *)
    true
  ) else if p2.startColumn > p1.startColumn then (
    (* same logic *)
    false
  ) else (
    (* equal start columns, compare nonterm ids *)
    let p1NtIndex = getProdInfo_lhsIndex queue.rpqTables p1.prodIndex in
    let p2NtIndex = getProdInfo_lhsIndex queue.rpqTables p2.prodIndex in

    (* check nonterm order *)
    let ord1 = getNontermOrdinal queue.rpqTables p1NtIndex in
    let ord2 = getNontermOrdinal queue.rpqTables p2NtIndex in

    ord1 < ord2
  )


let insertPathCopy queue src leftEdge =
  let rhsLen = getProdInfo_rhsLen queue.rpqTables src.prodIndex in

  (* make a new node *)
  let p = Objpool.alloc queue.pathPool in
  initPath p src.startStateId src.prodIndex rhsLen;

  (* fill in left edge info *)
  p.leftEdgeNode <- leftEdge;
  p.startColumn  <- leftEdge.column;

  (* copy path info *)
  Array.blit
    !(src.sibLinks)       (* source array *)
    0                     (* source start position *)
    !(p.sibLinks)         (* dest array *)
    0                     (* dest start position *)
    rhsLen;               (* number of elements to copy *)
  Array.blit
    !(src.symbols)        (* source array *)
    0                     (* source start position *)
    !(p.symbols)          (* dest array *)
    0                     (* dest start position *)
    rhsLen;               (* number of elements to copy *)

  (* find proper place to insert new path *)
  if not (isSome queue.top) || goesBefore queue p (getSome queue.top) then (
    (* prepend *)
    p.next <- queue.top;
    queue.top <- Some p;
  ) else (
    (* search *)
    let prev = ref (getSome queue.top) in
    while (isSome !prev.next) && not (goesBefore queue p (getSome !prev.next)) do
      prev := getSome !prev.next;
    done;

    (* insert *)
    p.next <- !prev.next;
    !prev.next <- Some p;
  )


(* same argument meanings as for 'rwlRecursiveEnqueue' *)
let rec rwlCollectPathLink glr proto popsRemaining currentNode mustUseLink linkToAdd =
  !(proto.sibLinks).(popsRemaining) <- linkToAdd;
  !(proto.symbols ).(popsRemaining) <- getNodeSymbol currentNode;

  rwlRecursiveEnqueue glr proto popsRemaining linkToAdd.sib (
    match mustUseLink with
    | Some link when link == linkToAdd ->
        None
    | _ ->
        (* consume must-use link *)
        mustUseLink
  )


(* recursive depth-first enumeration of paths *)
and rwlRecursiveEnqueue glr
  proto         (* prototype path, with path so far *)
  popsRemaining (* # of links yet to traverse to find a full path *)
  currentNode   (* node we're at in the path *)
  mustUseLink   (* link the path must use (if not None) *)
=
  if popsRemaining = 0 then (
    (* found path *)

    (* must have used the link *)
    match mustUseLink with
    | Some _ ->
        (* do nothing *)
        ()

    | None ->
        (* copy the prototype path, it's the one we want *)
        insertPathCopy glr.pathQueue proto currentNode

  ) else (

    (* explore currentNode's siblings *)
    rwlCollectPathLink glr proto (popsRemaining - 1) currentNode mustUseLink currentNode.firstSib;

    List.iter (fun sibling ->
      rwlCollectPathLink glr proto (popsRemaining - 1) currentNode mustUseLink sibling
    ) currentNode.leftSiblings
  )


let deletePath queue p =
  Objpool.dealloc queue.pathPool p


(* returns # of actions *)
let rec rwlEnqueueReductions glr parsr action mustUseLink =
  assert (checkLocalInvariants parsr);

  if isShiftAction glr.tables action then (
    (* do nothing, only looking for reductions *)
    1
  ) else if isReduceAction (*tables*) action then (
    let prodIndex = decodeReduce (*tables*) action parsr.state in

    (* production info *)
    let rhsLen = getProdInfo_rhsLen glr.tables prodIndex in
    assert (rhsLen >= 0);       (* paranoia *)

    (* make a prototype path; used to control recursion *)
    let proto = newPath glr.pathQueue parsr.state prodIndex rhsLen in

    (* kick off the recursion *)
    rwlRecursiveEnqueue glr proto rhsLen parsr mustUseLink;

    (* deallocate prototype *)
    deletePath glr.pathQueue proto;

    1
  ) else if isErrorAction (*tables*) action then (
    (* parser just dies *)
    0
  ) else (
    (* ambiguous; check for reductions in list of actions *)
    let firstEntry = decodeAmbigAction glr.tables action parsr.state in
    let numEntries = glr.tables.ambigTable.(firstEntry) in

    for i = 1 to numEntries do
      (* ignore return value because I know it will be 1 *)
      ignore (rwlEnqueueReductions glr parsr glr.tables.ambigTable.(firstEntry + i) mustUseLink);
    done;

    numEntries
  )


let queueIsNotEmpty queue =
  isSome queue.top


let dequeue queue =
  let ret = getSome queue.top in
  queue.top <- ret.next;
  ret


let doReductionAction glr productionId svals =
  glr.userAct.reductionAction productionId svals


let findTopmostParser glr state =
  (* always using the *not* USE_PARSER_INDEX case *)
  Arraystack.findOption (fun n -> n.state = state) glr.topmostParsers


let canMakeProgress tokType glr parsr =
  let entry = getActionEntry glr.tables parsr.state tokType in

  isShiftAction glr.tables entry
    || isReduceAction (*tables*) entry
    || not (isErrorAction (*tables*) entry)


(* ------------------------ RWL algorithm ------------------------- *)
let rwlShiftActive tokType glr leftSibling rightSibling lhsIndex sval =
  match (getLinkTo rightSibling leftSibling) with
  | Some sibLink ->
      (* we already have a sibling link, don't need a new one *)

      (* +--------------------------------------------------+
       * | it is here that we are bringing the tops of two  |
       * | alternative parses together (TREEBUILD)          |
       * +--------------------------------------------------+
       *)

      (* dead tree optimization *)
      if not (canMakeProgress tokType glr rightSibling) then (
        if traceParse then
          Printf.printf "avoided a merge by noticing the state was dead\n";
        deallocateSemanticValue (getNodeSymbol rightSibling) glr.userAct sval;
      ) else (
        (* call user's merge code *)
        sibLink.sval <- glr.userAct.mergeAlternativeParses lhsIndex sibLink.sval sval;
      );

      (* ok, done *)
      None

      (* didn't add a link, no potential for new paths *)

  | None ->
      (* no suitable sibling link already, so add it *)
      let sibLink = addSiblingLink rightSibling leftSibling sval in

      (* recompute depths; TODO: do the topological sort thing *)
      if rightSibling.referenceCount > 1 then (
        let changes = ref true in
        let iters   = ref 0 in

        while !changes do
          changes := false;
          Arraystack.iter glr.topmostParsers
            (fun parsr ->
              let newDepth = computeDeterminDepth parsr in
              if newDepth <> parsr.determinDepth then (
                changes := true;
                parsr.determinDepth <- newDepth;
              )
            );
          incr iters;
          assert (!iters < 1000);     (* protect against infinite loop *)
        done
      );

      (* inform caller of new link *)
      Some sibLink


let rwlShiftNew tokType glr leftSibling rightSiblingState sval =
  (* not already active parser in this state, so make one *)
  let rightSibling = makeStackNode glr rightSiblingState in

  (* add link *)
  ignore (addSiblingLink rightSibling leftSibling sval);

  (* extend frontier *)
  addTopmostParser glr rightSibling;

  (* enqueue this new parser's reductions *)
  let action = getActionEntry glr.tables rightSibling.state tokType in
  ignore (rwlEnqueueReductions glr rightSibling action None(*siblink*));

  (* caller doesn't need to do anything more *)
  None


let rwlShiftNonterminal tokType glr leftSibling lhsIndex sval =
  (* consult goto table to find where to go upon "shifting" the nonterminal *)
  let rightSiblingState =
    decodeGoto (getGotoEntry glr.tables leftSibling.state lhsIndex) lhsIndex
  in

  if traceParse then
    Printf.printf "state %d, shift nonterm %d, to state %d\n" leftSibling.state lhsIndex rightSiblingState;

  (* is there already an active parser with this state? *)
  match findTopmostParser glr rightSiblingState with
  | Some rightSibling ->
      rwlShiftActive tokType glr leftSibling rightSibling lhsIndex sval

  | None ->
      rwlShiftNew tokType glr leftSibling rightSiblingState sval


let rwlProcessWorklist tokType glr =
  while (queueIsNotEmpty glr.pathQueue) do
    (* process enabled reductions in priority order *)
    let path = dequeue glr.pathQueue in

    (* production info *)
    let rhsLen   = getProdInfo_rhsLen   glr.tables path.prodIndex in
    let lhsIndex = getProdInfo_lhsIndex glr.tables path.prodIndex in

    if traceParse then
      Printf.printf "state %d, reducing by production %d (rhsLen=%d), back to state %d\n"
                     path.startStateId
                     path.prodIndex
                     rhsLen
                     path.leftEdgeNode.state;

    if accounting then
      glr.nondetReduce <- glr.nondetReduce + 1;

    (* leftEdge *)

    (* before calling user's code, duplicate svals *)
    for i = rhsLen - 1 downto 0 do
      let sib = (!(path.sibLinks)).(i) in

      (* put the sval in the array that will be passed to the user *)
      glr.toPass.(i) <- sib.sval;

      (* source loc stuff *)

      (* ask user to duplicate, store that back in 'sib' *)
      sib.sval <- duplicateSemanticValue glr !(path.symbols).(i) sib.sval;
    done;

    (* invoke user's reduction action (TREEBUILD) *)
    let sval = doReductionAction glr path.prodIndex glr.toPass in

    (* did user want to keep? *)
    if use_keep && not (glr.userAct.keepNontermValue lhsIndex sval) then (
      (* cancelled; drop on floor *)
    ) else (
      (* shift the nonterminal, sval *)
      let newLink = rwlShiftNonterminal tokType glr path.leftEdgeNode lhsIndex sval in

      if isSome newLink then
        (* for each 'finished' parser, enqueue actions enabled by the new link *)
        Arraystack.iter glr.topmostParsers
          (fun parsr ->
            let action = getActionEntry glr.tables parsr.state tokType in
            ignore (rwlEnqueueReductions glr parsr action newLink)
          )
    );

    (* we dequeued it above, and are now done with it, so recycle
     * it for future use *)
    deletePath glr.pathQueue path
  done


let rwlShiftTerminals tokenKindDesc lexer glr =
  glr.globalNodeColumn <- glr.globalNodeColumn + 1;

  (* move all parsers from 'topmostParsers' to 'prevTopmost' *)
  assert (Arraystack.isEmpty glr.prevTopmost);
  Arraystack.swapWith glr.prevTopmost glr.topmostParsers;
  assert (Arraystack.isEmpty glr.topmostParsers);

  (* grab current token since we'll need it and the access
   * isn't all that fast here in ML *)
  let tokType = Lexerint.(lexer.tokType) in

  (* for token multi-yield.. *)
  let prev = ref None in

  while not (Arraystack.isEmpty glr.prevTopmost) do
    (* take the node from 'prevTopmost'; the refcount transfers
     * from 'prevTopmost' to (local variable) 'leftSibling' *)
    let leftSibling = Arraystack.pop glr.prevTopmost in
    assert (leftSibling.referenceCount >= 1);   (* for the local *)

    (* can this parser shift? *)
    let action = getActionEntry glr.tables leftSibling.state tokType in

    (* if we find a shift, this will be set to something valid *)
    let newState = ref cSTATE_INVALID in

    (* consult action table, looking for shifts *)
    if isShiftAction glr.tables action then (
      (* unambiguous shift *)
      newState := (decodeShift (*tables*) action tokType);
    ) else if isReduceAction (*tables*) action
           || isErrorAction (*tables*) action then (
      (* unambiguous reduction or error *)
    ) else (
      (* nondeterministic *)
      let firstEntry = decodeAmbigAction glr.tables action leftSibling.state in
      let numEntries = glr.tables.ambigTable.(firstEntry) in

      let i = ref 1 in
      while !i <> numEntries do
        let action = glr.tables.ambigTable.(firstEntry + !i) in
        incr i;
        if isShiftAction glr.tables action then (
          (* a shift was among the conflicted actions *)
          newState := decodeShift (*tables*) action tokType;

          (* "break" *)
          i := numEntries
        )
      done
    );

    if !newState <> cSTATE_INVALID then (
      (* found a shift *)

      if accounting then
        glr.nondetShift <- glr.nondetShift + 1;

      if traceParse then
        Printf.printf "state %d, shift token %s, to state %d\n"
                       leftSibling.state
                       (tokenKindDesc Lexerint.(lexer.tokType))
                       !newState;

      (* already a parser in this state? *)
      let rightSibling =
        match findTopmostParser glr !newState with
        | Some rs ->
            (* use existing *)
            rs
        | None ->
            (* must make a new stack node *)
            let rs = makeStackNode glr !newState in
            (* add it to active parsers *)
            addTopmostParser glr rs;
            (* use new *)
            rs
      in

      (* semantic value for this token *)
      let sval =
        match !prev with
        | None ->
            (* usual case *)
            Lexerint.(lexer.sval)

        | Some prev ->
            (* the 'sval' we just grabbed has already been claimed by
             * 'prev.sval'; get a fresh one by duplicating the latter *)
            glr.userAct.duplicateTerminalValue tokType prev.sval
      in

      (* add sibling link now *)
      prev := Some (addSiblingLink rightSibling leftSibling sval);

      (* see comments in glr.cc for explanation *)
      assert (rightSibling.referenceCount = 1);
    );

    (* pending decrement of leftSibling, which is about to go out of scope *)
    decRefCt leftSibling;
  done


let printParseErrorMessage tokenKindDesc tokType glr lastToDie =
  if not glr.noisyFailedParse then
    ()
  else begin
    if not (lastToDie = cSTATE_INVALID) then (
      Printf.printf "In state %d, I expected one of these tokens:\n" lastToDie;
      for i = 0 to glr.tables.numTerms - 1 do
        let act = getActionEntry glr.tables lastToDie i in
        if not (isErrorAction (*tables*) act) then
          Printf.printf "  [%d] %s\n" i (tokenKindDesc i);
      done
    ) else (
      Printf.printf "(expected-token info not available due to nondeterministic mode\n"
    );

    Printf.printf (*loc*) "Parse error (state %d) at %s\n"
                  lastToDie
                  (tokenKindDesc tokType);
  end


let nondeterministicParseToken tokenKindDesc lexer glr =
  let lastToDie = ref cSTATE_INVALID in

  (* seed the reduction worklist by analyzing the top nodes *)
  Arraystack.iter glr.topmostParsers (fun parsr ->
    let tt = Lexerint.(lexer.tokType) in
    let action = getActionEntry glr.tables parsr.state tt in
    let actions = rwlEnqueueReductions glr parsr action None(*sibLink*) in
    
    if actions = 0 then (
      if traceParse then
        Printf.printf "parser in state %d died\n" parsr.state;
      lastToDie := parsr.state
    )
  );

  (* drop into worklist processing loop *)
  rwlProcessWorklist Lexerint.(lexer.tokType) glr;

  (* do all shifts last *)
  rwlShiftTerminals tokenKindDesc lexer glr;

  (* error? *)
  if Arraystack.isEmpty glr.topmostParsers then (
    printParseErrorMessage tokenKindDesc Lexerint.(lexer.tokType) glr !lastToDie;
    false
  ) else (
    true
  )


let cleanupAfterParse glr treeTop =
  if traceParse then
    Printf.printf "Parse succeeded!\n";

  if not (Arraystack.length glr.topmostParsers = 1) then (
    Printf.printf "parsing finished with %d active parsers!\n"
                  (Arraystack.length glr.topmostParsers);
    false
  ) else (
    let last = Arraystack.top glr.topmostParsers in

    (* prepare to run final action *)
    let arr = Array.make 2 cNULL_SVAL in
    let nextToLast = (getUniqueLink last).sib in
    arr.(0) <- grabTopSval glr nextToLast;      (* sval we want *)
    arr.(1) <- grabTopSval glr last;            (* EOF's sval *)

    (* reduce *)
    treeTop := doReductionAction glr glr.tables.finalProductionIndex arr;

    (* before pool goes away.. *)
    decParserList glr.topmostParsers;

    true
  )


let pullFromTopmostParsers glr parsr =
  let last = Arraystack.length glr.topmostParsers - 1 in

  let i = ref 0 in
  while !i <> last do
    if Arraystack.elt glr.topmostParsers !i = parsr then (
      (* remove by swapping *)
      if !i < last then
        Arraystack.setElt glr.topmostParsers !i (Arraystack.elt glr.topmostParsers last);
      ignore (Arraystack.pop glr.topmostParsers);   (* removes a reference *)
      decRefCt parsr;                    (* so decrement *)
      i := last
    ) else (
      incr i
    )
  done


(* dumpGSS goes here *)
(* dumpGSSEdge goes here *)


let nodeSummary node =
  Printf.sprintf "%d[%d]" node.state node.referenceCount


let rec innerStackSummary printed node =
  if List.exists ((==) node) !printed then (
    (* already printed *)
    "(rep:" ^ nodeSummary node ^ ")"

  ) else (

    (* remember that we've now printed 'node' *)
    printed := node :: !printed;

    if node.firstSib.sib == cNULL_STACK_NODE then (
      (* no siblings *)
      nodeSummary node

    ) else if (isEmpty node.leftSiblings) then (
      (* one sibling *)
      nodeSummary node ^ "-" ^
      innerStackSummary printed node.firstSib.sib

    ) else (
      (* multiple siblings *)
      (* force order of eval *)
      let nodeSummary = nodeSummary node in
      let firstSummary = innerStackSummary printed node.firstSib.sib in
      let siblingsSummary =
        List.fold_left (fun acc link -> 
          acc ^ "|" ^ innerStackSummary printed link.sib
        ) "" node.leftSiblings
      in
      Printf.sprintf "%s-(%s%s)" nodeSummary firstSummary siblingsSummary
    )
  )


let stackSummary glr =
  (* nodes already printed *)
  let printed = ref [] in

  (* loop/fold *)
  let len = Arraystack.length glr.topmostParsers in
  let rec loop acc i =
    if i > len - 1 then
      (* done *)
      acc
    else
      let n = Arraystack.elt glr.topmostParsers i in
      let summary = Printf.sprintf "%s (%d: %s)"
        acc i (innerStackSummary printed n)
      in

      loop summary (i + 1)
  in

  loop "" 0


(* pulled out so I can use this block of statements in several places *)
let glrParseToken tokenKindDesc glr getToken lexer =
  if not (nondeterministicParseToken tokenKindDesc lexer glr) then
    raise Exit;              (* "return false" *)

  (* goto label: getNextToken *)
  (* last token? *)
  if Lexerint.(lexer.tokType) = 0 then
    raise End_of_file;       (* "break" *)

  (* get the next token *)
  getToken lexer


let rec lrParseToken tokenKindDesc glr lr getToken lexer =
  let parsr = ref (Arraystack.top glr.topmostParsers) in
  assert (!parsr.referenceCount = 1);

  let tok = Lexerint.(lexer.tokType) in
  let action = getActionEntry_noError glr.tables !parsr.state tok in

  if isReduceAction action then (
    if accounting then
      lr.lrDetReduce <- lr.lrDetReduce + 1;

    let prodIndex = decodeReduce action !parsr.state in
    let rhsLen = getProdInfo_rhsLen glr.tables prodIndex in

    if rhsLen <= !parsr.determinDepth then (
      (* can reduce unambiguously *)
      let lhsIndex = getProdInfo_lhsIndex glr.tables prodIndex in

      let startStateId = !parsr.state in

      assert (rhsLen <= cMAX_RHSLEN);

      (* loop for arbitrary rhsLen *)
      for i = rhsLen - 1 downto 0 do
        (* grab the (only) sibling of 'parsr' *)
        let sib = !parsr.firstSib in

        (* store its semantic value in the 'toPass' array *)
        lr.lrToPass.(i) <- sib.sval;

        (* pop 'parsr' and move to next one *)
        Objpool.dealloc glr.stackNodePool !parsr;
        let prev = !parsr in
        parsr := sib.sib;

        assert (!parsr.referenceCount = 1);
        assert (prev.referenceCount = 1);

        (* adjust a couple things about 'prev' reflecting
         * that it has been deallocated *)
        decr numStackNodesAllocd;
        prev.firstSib.sib <- cNULL_STACK_NODE;

        assert (!parsr.referenceCount = 1);
      done;

      (* call the user's action function (TREEBUILD) *)
      let sval =
        glr.userAct.reductionAction prodIndex lr.lrToPass
      in

      (* now, do an abbreviated 'glrShiftNonterminal' *)
      let newState =
        decodeGoto (getGotoEntry glr.tables !parsr.state lhsIndex) lhsIndex
      in

      if traceParse then (
        Printf.printf "state %d, (unambig) reduce by %d (len=%d), back to %d then out to %d\n"
                      startStateId
                      prodIndex
                      rhsLen
                      !parsr.state
                      newState;
        flush stdout;
      );

      (* the sole reference is the 'parsr' variable *)
      assert (!parsr.referenceCount = 1);

      (* push new state *)
      let newNode =
        makeStackNode glr newState
      in

      addFirstSiblingLink_noRefCt newNode !parsr sval;

      assert (!parsr.referenceCount = 1);

      (* replace old topmost parser with 'newNode' *)
      Arraystack.setElt glr.topmostParsers 0 newNode;
      incRefCt newNode;
      assert (newNode.referenceCount = 1);

      (* does the user want to keep it? *)
      if use_keep && not (glr.userAct.keepNontermValue lhsIndex sval) then (
        printParseErrorMessage tokenKindDesc Lexerint.(lexer.tokType) glr newNode.state;
        if accounting then (
          glr.detShift  <- glr.detShift  + lr.lrDetShift;
          glr.detReduce <- glr.detReduce + lr.lrDetReduce;
        );

        raise Exit               (* "return false" *)
      );

      (* we have not shifted a token, so again try to use
       * the deterministic core *)
      (* "goto tryDeterministic;" *)
      lrParseToken tokenKindDesc glr lr getToken lexer

    ) else (
      (* deterministic depth insufficient: use GLR *)
      glrParseToken tokenKindDesc glr getToken lexer
    )

  ) else if isShiftAction glr.tables action then (
    if accounting then
      lr.lrDetShift <- lr.lrDetShift + 1;

    (* can shift unambiguously *)
    let newState = decodeShift action tok in

    if traceParse then (
      Printf.printf "state %d, (unambig) shift token %d, to state %d\n"
                    !parsr.state
                    tok
                    newState;
      flush stdout;
    );

    glr.globalNodeColumn <- glr.globalNodeColumn + 1;

    let rightSibling =
      makeStackNode glr newState
    in

    addFirstSiblingLink_noRefCt rightSibling !parsr Lexerint.(lexer.sval);

    (* replace 'parsr' with 'rightSibling' *)
    Arraystack.setElt glr.topmostParsers 0 rightSibling;

    assert (!parsr.referenceCount = 1);
    assert (rightSibling.referenceCount = 0);

    rightSibling.referenceCount <- 1;

    (* get next token *)
    (* "goto getNextToken;" *)
    (* last token? *)
    if Lexerint.(lexer.tokType) = 0 then
      raise End_of_file;       (* "break" *)

    (* get the next token *)
    lrParseToken tokenKindDesc glr lr getToken (getToken lexer)

  ) else (
    (* error or ambig; not deterministic *)
    glrParseToken tokenKindDesc glr getToken lexer
  )


let rec main_loop tokenKindDesc glr lr getToken lexer =
  if traceParse then (
    Printf.printf "---- processing token %s, %d active parsers ----\n"
                   (tokenKindDesc Lexerint.(lexer.tokType))
                   (Arraystack.length glr.topmostParsers);
    Printf.printf "Stack:%s\n" (stackSummary glr);
    flush stdout
  );

  (* ------------ glr parser ------------ *)
  (* --------- mini-lr parser ------- *)
  (* see elkhound/glr.cc for more details *)
  if use_mini_lr && Arraystack.length glr.topmostParsers = 1 then 
    (* try deterministic parsing *)
    main_loop tokenKindDesc glr lr getToken (lrParseToken tokenKindDesc glr lr getToken lexer)
  else
    (* mini lr core disabled, use full GLR *)
    main_loop tokenKindDesc glr lr getToken (glrParseToken tokenKindDesc glr getToken lexer)


(* buildParserIndex goes here *)

let glrParse glr tokenKindDesc getToken lexer treeTop =
  glr.globalNodeColumn <- 0;
  begin
    let first = makeStackNode glr 0(*startState*) in
    addTopmostParser glr first;
  end;

  (* array for passing semantic values in the mini lr core *)
  let lr = {
    lrToPass    = Array.make cMAX_RHSLEN cNULL_SVAL;
    lrDetShift  = 0;
    lrDetReduce = 0;
  } in

  (* main parsing loop *)
  try

    main_loop tokenKindDesc glr lr getToken (getToken lexer)

  with
  | Exit ->
      false

  | End_of_file ->
      if accounting then (
        glr.detShift  <- glr.detShift  + lr.lrDetShift;
        glr.detReduce <- glr.detReduce + lr.lrDetReduce;
      );

      (* end of parse *)
      cleanupAfterParse glr treeTop
