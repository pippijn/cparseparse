(* Implementation Notes
 *
 * A design point: [GLR] uses more 'global's than I do.  My criteria
 * here is that something should be global (stored in class GLR) if
 * it has meaning between processing of tokens.  If something is only
 * used during the processing of a single token, then I make it a
 * parameter where necessary.
 *
 * Update: I've decided to make 'currentToken' and 'parserWorklist'
 * global because they are needed deep inside of 'glrShiftNonterminal',
 * though they are not needed by the intervening levels, and their
 * presence in the argument lists would therefore only clutter them.
 *
 * (OLD) It should be clear that many factors contribute to this
 * implementation being slow, and I'm going to refrain from any
 * optimization for a bit.
 *
 * UPDATE (3/29/02): I'm now trying to optimize it.  The starting
 * implementation is 300x slower than bison.  Ideal goal is 3x, but
 * more realistic is 10x.
 *
 * UPDATE (8/24/02): It's very fast now; within 3% of Bison for
 * deterministic grammars, and 5x when I disable the mini-LR core.
 *
 * Description of the various lists in play here:
 *
 *   topmostParsers
 *   --------------
 *   The active parsers are at the frontier of the parse tree
 *   space.  It *never* contains more than one stack node with
 *   a given parse state; I call this the unique-state property
 *   (USP).  If we're about to add a stack node with the same
 *   state as an existing node, we merge them (if it's a shift,
 *   we add another leftAdjState; if it's a reduction, we add a
 *   rule node *and* another leftAdjState).
 *
 *   Before a token is processed, topmostParsers contains those
 *   parsers that successfully shifted the previous token.  This
 *   list is then walked to make the initial reduction worklist.
 *
 *   Before the shifts are processed, the topmostParsers list is
 *   cleared.  As each shift is processed, the resulting parser is
 *   added to topmostParsers (modulo USP).
 *
 *   [GLR] calls this "active-parsers"
 *
 *
 * Discussion of path re-examination, called do-limited-reductions by
 * [GLR]:
 *
 * After thinking about this for some time, I have reached the conclusion
 * that the only way to handle the problem is to separate the collection
 * of paths from the iteration over them.
 *
 * Here are several alternative schemes, and the reasons they don't
 * work:
 *
 *   1. [GLR]'s approach of limiting re-examination to those involving
 *      the new link
 *
 *      This fails because it does not prevent re-examined paths
 *      from appearing in the normal iteration also.
 *
 *   2. Modify [GLR] so the new link can't be used after the re-examination
 *      is complete
 *
 *      Then if *another* new link is added, paths involving both new
 *      links wouldn't be processed.
 *
 *   3. Further schemes involving controlling which re-examination stage can
 *      use which links
 *
 *      Difficult to reason about, unclear a correct scheme exists, short
 *      of the full-blown path-listing approach I'm going to take.
 *
 *   4. My first "fix" which assumes there is never more than one path to
 *      a given parser
 *
 *      This is WRONG.  There can be more than one path, even as all such
 *      paths are labeled the same (namely, with the RHS symbols).  Consider
 *      grammar "E -> x | E + E" parsing "x+x+x": both toplevel parses use
 *      the "E -> E + E" rule, and both arrive at the root parser
 *
 * So, the solution I will implement is to collect all paths into a list
 * before processing any of them.  During path re-examination, I also will
 * collect paths into a list, this time only those that involve the new
 * link.
 *
 * This scheme is clearly correct, since path collection cannot be disrupted
 * by the process of adding links, and when links are added, exactly the new
 * paths are collected and processed.  It's easy to see that every path is
 * considered exactly once.
 *
 *
 * MAJOR UPDATE (12/06/02):  I've replaced the state worklist (SWL) core
 * used in all previous GLR implementations with a reduction worklist (RWL)
 * core.  This core is just as fast, but can be implemented to always
 * avoid the yield-then-merge problem for acyclic grammars.
 *
 *
 * Below, parse-tree building activity is marked "TREEBUILD".
 *)


(* some random utilities *)
let getSome = function
  | None -> failwith "getSome applied to None"
  | Some v -> v


(* Relative to C++ implementation, what is not done:
 *   - Table compression
 *   - Heavy testing of the mini-LR core
 *)


(* NOTE: in some cases, more detailed comments can be found in
 * elkhound/glr.h, as these data structures mirror the ones
 * defined there *)


(* We define our own versions of these exceptions, so that user code raising
 * the ones in Pervasives will not interfere with parser internals. *)
exception End_of_file
exception Exit
exception Cancel of string


let cancel reason =
  raise (Cancel reason)


type statistics = {
  mutable detShift : int;
  mutable detReduce : int;
  mutable nondetShift : int;
  mutable nondetReduce : int;
}


(* link from one stack node to another *)
type 'result sibling_link = {
  (* stack node we're pointing at; == cNULL_STACK_NODE if none *)
  mutable sib : 'result stack_node;

  (* semantic value on this link *)
  mutable sval : SemanticValue.t;

  (* source locations *)
  mutable start_p : Lexing.position;
  mutable end_p : Lexing.position;

  (* possible TODO: yield count *)
}

(* node in the GLR graph-structured stack; all fields are
 * mutable because these are stored in a pool for explicit re-use *)
and 'result stack_node = {
  (* for access to parser context in a few unusual situations *)
  glr : 'result glr;

  (* LR parser state when this node is at the top *)
  mutable state : ParseTablesType.state_id;

  (* pointers to adjacent (to the left) stack nodes *)
  (* possible TODO: put links into a pool so I can deallocate them *)
  mutable leftSiblings : 'result sibling_link list;

  (* logically first sibling in the sibling list; separated out
   * from 'leftSiblings' for performance reasons *)
  mutable firstSib : 'result sibling_link;

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
and 'result path = {
  (* array of sibling links, i.e. the path; 0th element is
   * leftmost link *)
  mutable sibLinks : 'result sibling_link array;

  (* corresponding array of symbol ids to interpret svals *)
  mutable symbols : ParseTablesType.symbol_id array;

  (* rightmost state's id *)
  mutable startStateId : ParseTablesType.state_id;

  (* production we're going to reduce with *)
  mutable prodIndex : int;

  (* column from leftmost stack node *)
  mutable startColumn : int;

  (* the leftmost stack node itself *)
  mutable leftEdgeNode : 'result stack_node;

  (* next path in dequeueing order *)
  mutable next : 'result path option;
}

(* priority queue of reduction paths *)
and 'result reduction_path_queue = {
  (* head of the list, first to dequeue *)
  mutable top : 'result path option;

  (* pool of path objects *)
  pathPool : 'result path Objpool.t;

  (* need our own copy of the tables pointer *)
  rpqTables : ParseTablesType.t;      (* name can't collide with glr.tables.. ! *)
}


(* GLR parser object *)
(* some mutable fields are for hack in 'makeGLR' *)
and 'result glr = {
  (* user-specified actions *)
  userAct : 'result UserActions.t;

  (* parse tables from the grammar *)
  tables : ParseTablesType.t;

  (* treat this as a local variable of rwlProcessWorklist, included
   * here just to avoid unnecessary repeated allocation *)
  toPass : SemanticValue.t array;

  (* reduction queue and pool *)
  pathQueue : 'result reduction_path_queue;

  (* set of topmost parser nodes *)
  topmostParsers : 'result stack_node Arraystack.t;

  (* swapped with 'topmostParsers' periodically, for performance reasons *)
  prevTopmost : 'result stack_node Arraystack.t;

  (* node allocation pool; shared with glrParse *)
  mutable stackNodePool : 'result stack_node Objpool.t;

  (* when true, print some diagnosis of failed parses *)
  noisyFailedParse : bool;

  (* current token number *)
  mutable globalNodeColumn : int;

  (* parser action statistics *)
  stats : statistics;
}

(* Mini-LR parser object *)
type tLR = {
  lrToPass : SemanticValue.t array;

  mutable lrDetShift : int;
  mutable lrDetReduce : int;
}



(* what follows is based on elkhound/glr.cc *)

(* maximum RHS length for mini-lr core *)
let cMAX_RHSLEN = 8
(* this is the length to make arrays which hold rhsLen many items
 * typically, but are growable *)
let cINITIAL_RHSLEN_SIZE = 8


(* ------------------ accounting statistics ----------------- *)
let numStackNodesAllocd = ref 0
let maxStackNodesAllocd = ref 0


(* ----------------- front ends to user code --------------- *)
let reductionAction userAct productionId svals start_p end_p =
  let open UserActions in
  userAct.reductionAction productionId svals start_p end_p


let mergeAlternativeParses userAct lhsIndex left right =
  let open UserActions in
  userAct.mergeAlternativeParses lhsIndex left right


let keepNontermValue userAct lhsIndex sval =
  let open UserActions in
  userAct.keepNontermValue lhsIndex sval


let duplicateTerminalValue userAct term sval =
  let open UserActions in
  userAct.duplicateTerminalValue term sval


let duplicateNontermValue userAct nonterm sval =
  let open UserActions in
  userAct.duplicateNontermValue nonterm sval


let duplicateSemanticValue userAct sym sval =
  assert (sym <> 0);

  (* the C++ implementation checks for NULL sval, but I don't think
   * that can be here in the ML version, and I'm not convinced the
   * check would even be safe *)

  if ParseTables.symIsTerm sym then
    duplicateTerminalValue userAct (ParseTables.symAsTerm sym) sval
  else
    duplicateNontermValue userAct (ParseTables.symAsNonterm sym) sval


let deallocateSemanticValue userAct sym sval =
  let open UserActions in
  assert (sym <> 0);

  if ParseTables.symIsTerm sym then
    userAct.deallocateTerminalValue (ParseTables.symAsTerm sym) sval
  else
    userAct.deallocateNontermValue (ParseTables.symAsNonterm sym) sval


let reclassifiedToken userAct lexer token =
  let open Lexerint in
  let open UserActions in
  (* get original type/sval/sloc *)
  let tokType = lexer.index token in
  let tokSval = lexer.sval token in
  let tokSloc = lexer.sloc token in

  (* reclassify type *)
  let tokType = userAct.reclassifyToken tokType tokSval in

  (* return all token properties *)
  tokType, tokSval, tokSloc


let terminalName userAct tokType =
  let open UserActions in
  if GlrOptions._terminal_names () then
    userAct.terminalName tokType
  else
    userAct.terminalAlias tokType


let cSTATE_INVALID = ParseTablesType.cSTATE_INVALID


(* --------------------- SiblingLink ----------------------- *)
(* NULL sibling link *)
let cNULL_SIBLING_LINK : 'result sibling_link = Obj.magic ()

let makeSiblingLink sib (sval, start_p, end_p) = { sib; sval; start_p; end_p; }


(* --------------------- StackNode -------------------------- *)
(* NULL stack node *)
let cNULL_STACK_NODE : 'result stack_node = Obj.magic ()


let make_stack_node glr = {
  glr            = glr;
  state          = cSTATE_INVALID;
  leftSiblings   = [];
  firstSib       = makeSiblingLink cNULL_STACK_NODE (SemanticValue.null, Lexing.dummy_pos, Lexing.dummy_pos);
  referenceCount = 0;
  determinDepth  = 0;
  column         = 0;
}


let getNodeSymbol node =
  ParseTables.getStateSymbol node.glr.tables node.state


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

  if GlrOptions._accounting () then (
    decr numStackNodesAllocd;
  )


and deallocSemanticValues node =
  (* explicitly deallocate siblings, so I can deallocate their
   * semantic values if necessary (this requires knowing the
   * associated symbol, which the sibling_links don't know) *)
  if node.firstSib.sib != cNULL_STACK_NODE then
    deallocateSemanticValue node.glr.userAct (getNodeSymbol node) node.firstSib.sval;

  List.iter (fun s ->
    deallocateSemanticValue node.glr.userAct (getNodeSymbol node) s.sval;

    (* this is implicit in the C++ version, due to Owner<> *)
    decRefCt s.sib
  ) node.leftSiblings;

  node.leftSiblings <- []


let initStackNode node st =
  node.state <- st;
  assert (node.leftSiblings == []);
  assert (node.firstSib.sib == cNULL_STACK_NODE);
  node.referenceCount <- 0;
  node.determinDepth <- 1;

  if GlrOptions._accounting () then (
    incr numStackNodesAllocd;
    if !numStackNodesAllocd > !maxStackNodesAllocd then
      maxStackNodesAllocd := !numStackNodesAllocd;
  )


let hasZeroSiblings node =
  node.firstSib.sib == cNULL_STACK_NODE


let hasOneSibling node =
  node.firstSib.sib != cNULL_STACK_NODE && node.leftSiblings == []


let hasMultipleSiblings node =
  node.leftSiblings != []


(* add the very first sibling *)
let addFirstSiblingLink_noRefCt node leftSib (sval, start_p, end_p) =
  assert (hasZeroSiblings node);

  (* my depth will be my new sibling's depth, plus 1 *)
  node.determinDepth <- leftSib.determinDepth + 1;

  (* we don't have any siblings yet; use embedded
   * don't update reference count of 'leftSib', instead caller must do so *)
  assert (node.firstSib.sib == cNULL_STACK_NODE);
  node.firstSib.sib <- leftSib;     (* update w/o refct *)

  node.firstSib.sval <- sval;
  node.firstSib.start_p <- start_p;
  node.firstSib.end_p <- end_p


(* pulled out of 'addSiblingLink' so I can inline addSiblingLink
 * without excessive object code bloat; the branch represented by
 * the code in this function is much less common *)
let addAdditionalSiblingLink node leftSib sval =
  (* there's currently at least one sibling, and now we're adding another;
   * right now, no other stack node should point at this one (if it does,
   * most likely will catch that when we use the stale info)
   *
   * now there is a second outgoing pointer *)
  node.determinDepth <- 0;

  (* this was implicit in the C++ verison *)
  incRefCt leftSib;

  let link = makeSiblingLink leftSib sval in
  node.leftSiblings <- link :: node.leftSiblings;

  link


(* add a new sibling by creating a new link *)
let addSiblingLink node leftSib sval =
  if node.firstSib.sib == cNULL_STACK_NODE then (
    addFirstSiblingLink_noRefCt node leftSib sval;

    (* manually increment leftSib's refct *)
    incRefCt leftSib;

    (* pointer to firstSib.. *)
    node.firstSib
  ) else (
    (* as best I can tell, x86 static branch prediction is simply
     * "conditional forward branches are assumed not taken", hence
     * the uncommon case belongs in the 'else' branch *)
    addAdditionalSiblingLink node leftSib sval
  )


let getUniqueLink node =
  assert (hasOneSibling node);
  node.firstSib


let getLinkTo node another =
  (* check first.. *)
  if node.firstSib.sib == another then (
    Some node.firstSib
  ) else (
    (* check rest *)
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
    (* it must be equal to sibling's, plus one *)
    node.firstSib.sib.determinDepth + 1
  ) else (
    assert (hasMultipleSiblings node);
    0
  )


let checkLocalInvariants node =
  computeDeterminDepth node = node.determinDepth


(* ----------------------------- GLR --------------------------------- *)
let makePath () = {
  startStateId = cSTATE_INVALID;
  prodIndex    = -1;
  startColumn  = -1;
  leftEdgeNode = cNULL_STACK_NODE;
  sibLinks     = Array.make cINITIAL_RHSLEN_SIZE cNULL_SIBLING_LINK;
  symbols      = Array.make cINITIAL_RHSLEN_SIZE 0;
  next         = None;
}


let makeReductionPathQueue tables = {
  top = None;
  pathPool = Objpool.make makePath;
  rpqTables = tables;
}


let makeGLR userAct tables =
  let glr = {
    userAct;
    tables;
    toPass              = Array.make cMAX_RHSLEN SemanticValue.null;
    pathQueue           = makeReductionPathQueue tables;
    topmostParsers      = Arraystack.create ();
    prevTopmost         = Arraystack.create ();
    stackNodePool       = Objpool.null ();
    noisyFailedParse    = true;
    globalNodeColumn    = 0;
    stats = {
      detShift          = 0;
      detReduce         = 0;
      nondetShift       = 0;
      nondetReduce      = 0;
    };
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

  glr.stackNodePool <- Objpool.make (fun () -> make_stack_node glr);

  (* the ordinary GLR core doesn't have this limitation because
   * it uses a growable array *)
  if GlrOptions._use_mini_lr () then
    (* make sure none of the productions have right-hand sides
     * that are too long; I think it's worth doing an iteration
     * here since going over the limit would be really hard to
     * debug, and this ctor is of course outside the main
     * parsing loop *)
    for i = 0 to ParseTables.getNumProds glr.tables - 1 do
      let len = ParseTables.getProdInfo_rhsLen glr.tables i in

      if len > cMAX_RHSLEN then (
        (* I miss token concatenation...*)
        Printf.printf "Production %d contains %d right-hand side symbols,\n" i len;
        Printf.printf "but the GLR core has been compiled with a limit of %d.\n" cMAX_RHSLEN;
        Printf.printf "Please adjust cMAX_RHSLEN and recompile the GLR core.\n";
        failwith "cannot continue";
      )
    done;

  glr


(**********************************************************
 * :: RWL algorithm
 **********************************************************)


let makeStackNode glr state =
  let dest = Objpool.alloc glr.stackNodePool in
  initStackNode dest state;
  dest.column <- glr.globalNodeColumn;
  dest


(* add a new parser to the 'topmostParsers' list, maintaining
 * related invariants*)
let addTopmostParser glr parsr =
  assert (checkLocalInvariants parsr);

  Arraystack.push parsr glr.topmostParsers;
  incRefCt parsr


(* stackTraceString *)

let initPath path ssi pi rhsLen =
  path.startStateId <- ssi;
  path.prodIndex <- pi;

  (* ensure the array has at least the given index, growing its size
   * if necessary (by doubling) *)
  while Array.length path.sibLinks < rhsLen + 1 do
    path.sibLinks <- Arraystack.growArray path.sibLinks (Array.length path.sibLinks * 2);
  done;

  while Array.length path.symbols < rhsLen + 1 do
    path.symbols <- Arraystack.growArray path.symbols (Array.length path.symbols * 2);
  done


let newPath queue ssi pi rhsLen =
  let p = Objpool.alloc queue.pathPool in
  initPath p ssi pi rhsLen;
  p

let deletePath queue p =
  Objpool.dealloc queue.pathPool p


let goesBefore queue p1 p2 =
  if p1.startColumn > p2.startColumn then (
    (* 'p1' spans fewer tokens, so it goes first *)
    true
  ) else if p2.startColumn > p1.startColumn then (
    (* same logic *)
    false
  ) else (
    (* equal start columns, compare nonterm ids *)
    let p1NtIndex = ParseTables.getProdInfo_lhsIndex queue.rpqTables p1.prodIndex in
    let p2NtIndex = ParseTables.getProdInfo_lhsIndex queue.rpqTables p2.prodIndex in

    (* check nonterm order *)
    let ord1 = ParseTables.getNontermOrdinal queue.rpqTables p1NtIndex in
    let ord2 = ParseTables.getNontermOrdinal queue.rpqTables p2NtIndex in

    ord1 < ord2
  )


let insertPathCopy queue src leftEdge =
  let rhsLen = ParseTables.getProdInfo_rhsLen queue.rpqTables src.prodIndex in

  (* make a new node *)
  let p = Objpool.alloc queue.pathPool in
  initPath p src.startStateId src.prodIndex rhsLen;

  (* fill in left edge info *)
  p.leftEdgeNode <- leftEdge;
  p.startColumn  <- leftEdge.column;

  (* copy path info *)
  Array.blit
    src.sibLinks          (* source array *)
    0                     (* source start position *)
    p.sibLinks            (* dest array *)
    0                     (* dest start position *)
    rhsLen;               (* number of elements to copy *)
  Array.blit
    src.symbols           (* source array *)
    0                     (* source start position *)
    p.symbols             (* dest array *)
    0                     (* dest start position *)
    rhsLen;               (* number of elements to copy *)

  (* find proper place to insert new path *)
  if queue.top == None || goesBefore queue p (getSome queue.top) then (
    (* prepend *)
    p.next <- queue.top;
    queue.top <- Some p;
  ) else (
    (* search *)
    let prev = ref (getSome queue.top) in
    while (!prev.next != None) && not (goesBefore queue p (getSome !prev.next)) do
      prev := getSome !prev.next;
    done;

    (* insert *)
    p.next <- !prev.next;
    !prev.next <- Some p;
  )


(* same argument meanings as for 'rwlRecursiveEnqueue' *)
let rec rwlCollectPathLink glr proto popsRemaining currentNode mustUseLink linkToAdd =
  proto.sibLinks.(popsRemaining) <- linkToAdd;
  proto.symbols .(popsRemaining) <- getNodeSymbol currentNode;

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


let rwlEnqueueReduceAction glr parsr action mustUseLink =
  let prodIndex = ParseTables.decodeReduce (*tables*) action parsr.state in

  (* production info *)
  let rhsLen = ParseTables.getProdInfo_rhsLen glr.tables prodIndex in
  assert (rhsLen >= 0);       (* paranoia *)

  (* make a prototype path; used to control recursion *)
  let proto = newPath glr.pathQueue parsr.state prodIndex rhsLen in

  (* kick off the recursion *)
  rwlRecursiveEnqueue glr proto rhsLen parsr mustUseLink;

  (* deallocate prototype *)
  deletePath glr.pathQueue proto


(* returns # of actions *)
let rec rwlEnqueueReductions glr parsr action mustUseLink =
  assert (checkLocalInvariants parsr);

  if ParseTables.isShiftAction glr.tables action then (
    (* do nothing, only looking for reductions *)
    1
  ) else if ParseTables.isReduceAction (*tables*) action then (
    rwlEnqueueReduceAction glr parsr action mustUseLink;
    1
  ) else if ParseTables.isErrorAction (*tables*) action then (
    (* parser just dies *)
    0
  ) else (
    (* ambiguous; check for reductions in list of actions *)
    let firstEntry = ParseTables.decodeAmbigAction glr.tables action parsr.state in
    let numEntries = ParseTables.getAmbigEntry glr.tables firstEntry in

    for i = 1 to numEntries do
      let entry = ParseTables.getAmbigEntry glr.tables (firstEntry + i) in
      (* ignore return value because I know it will be 1 *)
      ignore (rwlEnqueueReductions glr parsr entry mustUseLink);
    done;

    numEntries
  )


let queueIsNotEmpty queue =
  queue.top != None


let dequeue queue =
  let ret = getSome queue.top in
  queue.top <- ret.next;
  ret


let findTopmostParser glr state =
  (* always using the *not* USE_PARSER_INDEX case *)
  Arraystack.find (fun n -> n.state = state) glr.topmostParsers


let canMakeProgress glr tokType parsr =
  let entry = ParseTables.getActionEntry glr.tables parsr.state tokType in

  ParseTables.isShiftAction glr.tables entry
    || ParseTables.isReduceAction (*tables*) entry
    || not (ParseTables.isErrorAction (*tables*) entry)


let rwlShiftActive glr tokType leftSibling rightSibling lhsIndex sval =
  match (getLinkTo rightSibling leftSibling) with
  | Some sibLink ->
      (* we already have a sibling link, don't need a new one *)

      (* +--------------------------------------------------+
       * | it is here that we are bringing the tops of two  |
       * | alternative parses together (TREEBUILD)          |
       * +--------------------------------------------------+
       *)

      let sval, _, _ = sval in

      (* dead tree optimisation *)
      if not (canMakeProgress glr tokType rightSibling) then (
        if GlrOptions._trace_parse () then
          Printf.printf "avoided a merge by noticing the state was dead\n";
        deallocateSemanticValue glr.userAct (getNodeSymbol rightSibling) sval;
      ) else (
        (* call user's merge code *)
        sibLink.sval <- mergeAlternativeParses glr.userAct lhsIndex sibLink.sval sval;
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
          Arraystack.iter (fun parsr ->
            let newDepth = computeDeterminDepth parsr in
            if newDepth <> parsr.determinDepth then (
              changes := true;
              parsr.determinDepth <- newDepth;
            )
          ) glr.topmostParsers;
          incr iters;
          assert (!iters < 1000);     (* protect against infinite loop *)
        done
      );

      (* inform caller of new link *)
      Some sibLink


let rwlShiftNew glr tokType leftSibling rightSiblingState sval =
  (* not already active parser in this state, so make one *)
  let rightSibling = makeStackNode glr rightSiblingState in

  (* add link *)
  ignore (addSiblingLink rightSibling leftSibling sval);

  (* extend frontier *)
  addTopmostParser glr rightSibling;

  (* enqueue this new parser's reductions *)
  let action = ParseTables.getActionEntry glr.tables rightSibling.state tokType in
  ignore (rwlEnqueueReductions glr rightSibling action None(*siblink*));

  (* caller doesn't need to do anything more *)
  None


let rwlShiftNonterminal glr tokType leftSibling lhsIndex sval =
  (* consult goto table to find where to go upon "shifting" the nonterminal *)
  let rightSiblingState =
    ParseTables.getGoto glr.tables leftSibling.state lhsIndex
  in

  if GlrOptions._trace_parse () then
    Printf.printf "state %d, shift nonterm %d, to state %d\n" leftSibling.state lhsIndex rightSiblingState;

  (* is there already an active parser with this state? *)
  match findTopmostParser glr rightSiblingState with
  | Some rightSibling ->
      rwlShiftActive glr tokType leftSibling rightSibling lhsIndex sval

  | None ->
      rwlShiftNew glr tokType leftSibling rightSiblingState sval


let rwlProcessWorklist glr tokType tokSloc =
  while (queueIsNotEmpty glr.pathQueue) do
    (* process the enabled reductions in priority order *)
    let path = dequeue glr.pathQueue in

    (* info about the production *)
    let rhsLen   = ParseTables.getProdInfo_rhsLen   glr.tables path.prodIndex in
    let lhsIndex = ParseTables.getProdInfo_lhsIndex glr.tables path.prodIndex in

    if GlrOptions._trace_parse () then
      Printf.printf "state %d, reducing by production %d (rhsLen=%d), back to state %d\n"
                     path.startStateId
                     path.prodIndex
                     rhsLen
                     path.leftEdgeNode.state;

    if GlrOptions._accounting () then
      glr.stats.nondetReduce <- glr.stats.nondetReduce + 1;

    (* record location of left edge; initially is location of
     * the lookahead token *)
    let leftEdge = ref (fst tokSloc) in
    let rightEdge = ref Lexing.dummy_pos in

    (* before calling the user, duplicate any needed values *)
    for i = rhsLen - 1 downto 0 do
      let sib = path.sibLinks.(i) in

      (* put the sval in the array that will be passed to the user *)
      glr.toPass.(i) <- sib.sval;

      if sib.start_p != Lexing.dummy_pos then
        leftEdge := sib.start_p;
      if !rightEdge == Lexing.dummy_pos && sib.end_p != Lexing.dummy_pos then
        rightEdge := sib.end_p;

      (* ask user to duplicate, store that back in 'sib' *)
      sib.sval <- duplicateSemanticValue glr.userAct path.symbols.(i) sib.sval;
    done;

    (* invoke user's reduction action (TREEBUILD) *)
    let sval, keep =
      try
        let sval = reductionAction glr.userAct path.prodIndex glr.toPass !leftEdge !rightEdge in
        let keep = not (GlrOptions._use_keep ()) || keepNontermValue glr.userAct lhsIndex sval in
       
        sval, keep
      with Cancel reason ->
        SemanticValue.null, false
    in

    (* did user want to keep? *)
    if not keep then (
      (* cancelled; drop on floor *)
    ) else (
      (* add source locations *)
      let sval = sval, !leftEdge, !rightEdge in

      (* shift the nonterminal, sval *)
      let newLink = rwlShiftNonterminal glr tokType path.leftEdgeNode lhsIndex sval in

      if newLink != None then
        (* for each 'finished' parser, enqueue actions enabled by the new link *)
        Arraystack.iter (fun parsr ->
          let action = ParseTables.getActionEntry glr.tables parsr.state tokType in
          ignore (rwlEnqueueReductions glr parsr action newLink)
        ) glr.topmostParsers
    );

    (* we dequeued it above, and are now done with it, so recycle
     * it for future use *)
    deletePath glr.pathQueue path
  done


let rwlFindShift tables tokType action state =
  (* consult action table, looking for shifts *)
  if ParseTables.isShiftAction tables action then (
    (* unambiguous shift *)
    ParseTables.decodeShift (*tables*) action tokType
  ) else if ParseTables.isReduceAction (*tables*) action
         || ParseTables.isErrorAction (*tables*) action then (
    (* unambiguous reduction or error *)
    cSTATE_INVALID
  ) else (
    (* nondeterministic *)
    let firstEntry = ParseTables.decodeAmbigAction tables action state in
    let numEntries = ParseTables.getAmbigEntry tables firstEntry in

    let newState = ref cSTATE_INVALID in
    let i = ref 1 in
    while !i <> numEntries do
      let action = ParseTables.getAmbigEntry tables (firstEntry + !i) in
      incr i;
      if ParseTables.isShiftAction tables action then (
        (* a shift was among the conflicted actions *)
        newState := ParseTables.decodeShift (*tables*) action tokType;

        (* "break" *)
        i := numEntries
      )
    done;

    !newState
  )


let rwlShiftTerminals glr tokType tokSval tokSloc =
  glr.globalNodeColumn <- glr.globalNodeColumn + 1;

  (* move all parsers from 'topmostParsers' to 'prevTopmost' *)
  assert (Arraystack.is_empty glr.prevTopmost);
  Arraystack.swap glr.prevTopmost glr.topmostParsers;
  assert (Arraystack.is_empty glr.topmostParsers);

  (* for token multi-yield.. *)
  let prev = ref None in

  while not (Arraystack.is_empty glr.prevTopmost) do
    (* take the node from 'prevTopmost'; the refcount transfers
     * from 'prevTopmost' to (local variable) 'leftSibling' *)
    let leftSibling = Arraystack.pop glr.prevTopmost in
    assert (leftSibling.referenceCount >= 1);   (* for the local *)
    let state = leftSibling.state in

    (* can this parser shift? *)
    let action = ParseTables.getActionEntry glr.tables state tokType in

    (* if we find a shift, this will be set to something valid *)
    let newState = rwlFindShift glr.tables tokType action state in

    if newState <> cSTATE_INVALID then (
      (* found a shift *)

      if GlrOptions._accounting () then
        glr.stats.nondetShift <- glr.stats.nondetShift + 1;

      if GlrOptions._trace_parse () then
        Printf.printf "state %d, shift token %s, to state %d\n"
                       state
                       (terminalName glr.userAct tokType)
                       newState;

      (* already a parser in this state? *)
      let rightSibling =
        match findTopmostParser glr newState with
        | Some rs ->
            (* use existing *)
            rs
        | None ->
            (* must make a new stack node *)
            let rs = makeStackNode glr newState in
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
            let start_p, end_p = tokSloc in
            tokSval, start_p, end_p

        | Some prev ->
            (* the 'sval' we just grabbed has already been claimed by
             * 'prev.sval'; get a fresh one by duplicating the latter *)
            duplicateTerminalValue glr.userAct tokType prev.sval, prev.start_p, prev.end_p
      in

      (* add sibling link now *)
      prev := Some (addSiblingLink rightSibling leftSibling sval);

      (* adding this sibling link cannot violate the determinDepth
       * invariant of some other node, because all of the nodes created
       * or added-to during shifting do not have anything pointing at
       * them, so in particular nothing points to 'rightSibling'; a simple
       * check of this is to check the reference count and verify it is 1,
       * the 1 being for the 'topmostParsers' list it is on *)
      assert (rightSibling.referenceCount = 1);
    );

    (* pending decrement of leftSibling, which is about to go out of scope *)
    decRefCt leftSibling;
  done


(**********************************************************
 * :: Non-deterministic parser core
 **********************************************************)

let printParseErrorMessage ?reason glr tokType tokSloc lastToDie =
  if not glr.noisyFailedParse then
    ()
  else begin
    let start_p, end_p = tokSloc in

    if lastToDie <> cSTATE_INVALID then (
      Printf.printf "In state %d, I expected one of these tokens:\n" lastToDie;
      for i = 0 to ParseTables.getNumTerms glr.tables - 1 do
        let act = ParseTables.getActionEntry glr.tables lastToDie i in
        if not (ParseTables.isErrorAction (*tables*) act) then
          Printf.printf "  [%d] %s\n" i (terminalName glr.userAct i);
      done
    ) else (
      Printf.printf "(expected-token info not available due to nondeterministic mode)\n"
    );

    let open Lexing in
    Printf.printf (*loc*) "Parse error (state %d) at %s (%d:%d)\n"
                  lastToDie
                  (terminalName glr.userAct tokType)
                  start_p.pos_lnum
                  (start_p.pos_cnum - start_p.pos_bol);
    match reason with
    | None -> ()
    | Some reason ->
        Printf.printf "Last reduction was cancelled because: %s\n" reason
  end


let nondeterministicParseToken glr tokType tokSval tokSloc =
  let lastToDie = ref cSTATE_INVALID in

  (* seed the reduction worklist by analysing the top nodes *)
  Arraystack.iter (fun parsr ->
    let action = ParseTables.getActionEntry glr.tables parsr.state tokType in
    let actions = rwlEnqueueReductions glr parsr action None(*sibLink*) in

    if actions = 0 then (
      if GlrOptions._trace_parse () then
        Printf.printf "parser in state %d died\n" parsr.state;
      lastToDie := parsr.state
    )
  ) glr.topmostParsers;

  (* drop into worklist processing loop *)
  rwlProcessWorklist glr tokType tokSloc;

  (* do all shifts last *)
  rwlShiftTerminals glr tokType tokSval tokSloc;

  (* error? *)
  if Arraystack.is_empty glr.topmostParsers then (
    printParseErrorMessage glr tokType tokSloc !lastToDie;
    false
  ) else (
    true
  )


(* pulled out so I can use this block of statements in several places *)
let glrParseToken glr lexer token =
  let open Lexerint in

  (* grab current token since we'll need it and the access
   * isn't all that fast here in ML *)
  let tokType, tokSval, tokSloc = reclassifiedToken glr.userAct lexer token in

  if not (nondeterministicParseToken glr tokType tokSval tokSloc) then
    raise Exit;              (* "return false" *)

  (* goto label: getNextToken *)
  (* last token? *)
  if tokType = 0 then
    raise End_of_file;       (* "break" *)

  (* get the next token *)
  lexer.token ()


(**********************************************************
 * :: Mini-LR core
 **********************************************************)

let rec lrParseToken glr lr lexer token =
  let parsr = ref (Arraystack.top glr.topmostParsers) in
  assert (!parsr.referenceCount = 1);

  let tokType, tokSval, tokSloc = reclassifiedToken glr.userAct lexer token in

  let action = ParseTables.getActionEntry_noError glr.tables !parsr.state tokType in

  if ParseTables.isReduceAction action then (
    (* can reduce unambiguously *)
    let prodIndex = ParseTables.decodeReduce action !parsr.state in
    if GlrOptions._accounting () then
      lr.lrDetReduce <- lr.lrDetReduce + 1;

    let rhsLen = ParseTables.getProdInfo_rhsLen glr.tables prodIndex in

    if rhsLen <= !parsr.determinDepth then (
      let lhsIndex = ParseTables.getProdInfo_lhsIndex glr.tables prodIndex in

      let startStateId = !parsr.state in

      assert (rhsLen <= cMAX_RHSLEN);

      let leftEdge = ref (fst tokSloc) in
      let rightEdge = ref Lexing.dummy_pos in

      (* --- loop for arbitrary rhsLen ---
       * pop off 'rhsLen' stack nodes, collecting as many semantic
       * values into 'toPass'
       * NOTE: this loop is the innermost inner loop of the entire
       * parser engine -- even *one* branch inside the loop body
       * costs about 30% end-to-end performance loss! *)
      for i = rhsLen - 1 downto 0 do
        (* grab the (only) sibling of 'parsr' *)
        let sib = !parsr.firstSib in

        (* Store its semantic value it into array that will be
         * passed to user's routine.  Note that there is no need to
         * dup() this value, since it will never be passed to
         * another action routine (avoiding that overhead is
         * another advantage to the LR mode). *)
        lr.lrToPass.(i) <- sib.sval;

        (* if it has a valid source location, grab it *)
        if sib.start_p != Lexing.dummy_pos then
          leftEdge := sib.start_p;
        if !rightEdge == Lexing.dummy_pos && sib.end_p != Lexing.dummy_pos then
          rightEdge := sib.end_p;

        (* pop 'parsr' and move to next one *)
        Objpool.dealloc glr.stackNodePool !parsr;
        let prev = !parsr in
        parsr := sib.sib;

        assert (!parsr.referenceCount = 1);
        assert (prev.referenceCount = 1);

        (* adjust a couple things about 'prev' reflecting
         * that it has been deallocated *)
        if GlrOptions._accounting () then (
          decr numStackNodesAllocd;
        );
        prev.firstSib.sib <- cNULL_STACK_NODE;

        assert (!parsr.referenceCount = 1);
      done;

      (* call the user's action function (TREEBUILD) *)
      let sval, keep, reason =
        try
          let sval = reductionAction glr.userAct prodIndex lr.lrToPass !leftEdge !rightEdge in
          let keep = not (GlrOptions._use_keep ()) || keepNontermValue glr.userAct lhsIndex sval in

          sval, keep, ""
        with Cancel reason ->
          SemanticValue.null, false, reason
      in

      (* now, do an abbreviated 'glrShiftNonterminal' *)
      let newState =
        ParseTables.getGoto glr.tables !parsr.state lhsIndex
      in

      if GlrOptions._trace_parse () then (
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

      addFirstSiblingLink_noRefCt newNode !parsr (sval, !leftEdge, !rightEdge);

      assert (!parsr.referenceCount = 1);

      (* replace old topmost parser with 'newNode' *)
      assert (Arraystack.length glr.topmostParsers = 1);
      Arraystack.set glr.topmostParsers 0 newNode;
      incRefCt newNode;
      assert (newNode.referenceCount = 1);

      (* does the user want to keep it? *)
      if not keep then (
        printParseErrorMessage ~reason glr tokType tokSloc newNode.state;
        if GlrOptions._accounting () then (
          glr.stats.detShift  <- glr.stats.detShift  + lr.lrDetShift;
          glr.stats.detReduce <- glr.stats.detReduce + lr.lrDetReduce;
        );

        raise Exit               (* "return false" *)
      );

      (* we have not shifted a token, so again try to use
       * the deterministic core *)
      (* "goto tryDeterministic;" *)
      lrParseToken glr lr lexer token
    ) else (
      (* deterministic depth insufficient: use GLR *)
      glrParseToken glr lexer token
    )

  ) else if ParseTables.isShiftAction glr.tables action then (
    (* can shift unambiguously *)
    let newState = ParseTables.decodeShift action tokType in
    if GlrOptions._accounting () then
      lr.lrDetShift <- lr.lrDetShift + 1;

    if GlrOptions._trace_parse () then (
      Printf.printf "state %d, (unambig) shift token %d, to state %d\n"
                    !parsr.state
                    tokType
                    newState;
      flush stdout;
    );

    glr.globalNodeColumn <- glr.globalNodeColumn + 1;

    let rightSibling = makeStackNode glr newState in

    addFirstSiblingLink_noRefCt rightSibling !parsr (tokSval, fst tokSloc, snd tokSloc);

    (* replace 'parsr' with 'rightSibling' *)
    assert (Arraystack.length glr.topmostParsers = 1);
    Arraystack.set glr.topmostParsers 0 rightSibling;

    assert (!parsr.referenceCount = 1);
    assert (rightSibling.referenceCount = 0);

    rightSibling.referenceCount <- 1;

    (* get next token *)
    (* "goto getNextToken;" *)
    (* last token? *)
    if tokType = 0 then
      raise End_of_file;       (* "break" *)

    (* get the next token *)
    lrParseToken glr lr lexer Lexerint.(lexer.token ())

  ) else (
    (* error or ambig; not deterministic *)
    glrParseToken glr lexer token
  )


(**********************************************************
 * :: Debugging and tracing
 **********************************************************)

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

    ) else if node.leftSiblings == [] then (
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
      let n = Arraystack.nth glr.topmostParsers i in
      let summary = Printf.sprintf "%s (%d: %s)"
        acc i (innerStackSummary printed n)
      in

      loop summary (i + 1)
  in

  loop "" 0


(**********************************************************
 * :: Main parser loop
 **********************************************************)

(* This function is the core of the parser, and its performance is
 * critical to the end-to-end performance of the whole system. *)
let rec main_loop glr lr lexer token =
  if GlrOptions._trace_parse () then (
    let open Lexerint in
    let tokType = lexer.index token in

    Printf.printf "---- processing token %s, %d active parsers ----\n"
                   (terminalName glr.userAct tokType)
                   (Arraystack.length glr.topmostParsers);
    Printf.printf "Stack:%s\n" (stackSummary glr);
    flush stdout
  );

  if GlrOptions._use_mini_lr () && Arraystack.length glr.topmostParsers = 1 then
    (* try deterministic parsing *)
    main_loop glr lr lexer (lrParseToken glr lr lexer token)
  else
    (* mini lr core disabled, use full GLR *)
    main_loop glr lr lexer (glrParseToken glr lexer token)


(**********************************************************
 * :: Entry and exit of GLR parser
 **********************************************************)

(* used to extract the svals from the nodes just under the
 * start symbol reduction *)
let grabTopSval userAct node =
  let sib = getUniqueLink node in
  let ret = sib.sval in
  sib.sval <- duplicateSemanticValue userAct (getNodeSymbol node) sib.sval;

  (* TRSACTION("dup'd " << ret << " for top sval, yielded " << sib->sval); *)

  ret


let cleanupAfterParse (glr : 'result glr) : 'result option =
  if GlrOptions._trace_parse () then
    Printf.printf "Parse succeeded!\n";

  if not (Arraystack.length glr.topmostParsers = 1) then (
    Printf.printf "parsing finished with %d active parsers!\n"
                  (Arraystack.length glr.topmostParsers);
    None
  ) else (
    let last = Arraystack.top glr.topmostParsers in

    (* prepare to run final action *)
    let arr = Array.make 2 SemanticValue.null in
    let nextToLast = (getUniqueLink last).sib in
    arr.(0) <- grabTopSval glr.userAct nextToLast;      (* sval we want *)
    arr.(1) <- grabTopSval glr.userAct last;            (* EOF's sval *)

    (* reduce *)
    let finalProductionIndex = ParseTables.getFinalProductionIndex glr.tables in
    let treeTop = SemanticValue.obj (
      reductionAction glr.userAct finalProductionIndex arr
        Lexing.dummy_pos Lexing.dummy_pos) in

    (* before pool goes away.. *)
    Arraystack.iter decRefCt glr.topmostParsers;

    Some treeTop
  )


let glrParse (glr : 'result glr) lexer : 'result option =
  glr.globalNodeColumn <- 0;
  begin
    let startState = ParseTables.getStartState glr.tables in
    let first = makeStackNode glr startState in
    addTopmostParser glr first;
  end;

  (* array for passing semantic values in the mini lr core *)
  let lr = {
    lrToPass    = Array.make cMAX_RHSLEN SemanticValue.null;
    lrDetShift  = 0;
    lrDetReduce = 0;
  } in

  (* main parsing loop *)
  try

    (* this loop never returns normally *)
    Valgrind.Callgrind.instrumented
      (main_loop glr lr lexer) Lexerint.(lexer.token ())

  with
  | Exit ->
      None

  | End_of_file ->
      if GlrOptions._accounting () then (
        glr.stats.detShift  <- glr.stats.detShift  + lr.lrDetShift;
        glr.stats.detReduce <- glr.stats.detReduce + lr.lrDetReduce;
      );

      (* end of parse *)
      cleanupAfterParse glr
