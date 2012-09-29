(* glr.ml *)
(* GLR parser *)
(* based on elkhound/glr.h and elkhound/glr.cc *)

open Useract         (* tSemanticValue *)
open Parsetables     (* action/goto/etc. *)
open Smutil          (* getSome, etc. *)
open Lexerint


(* Relative to C++ implementation, what is not done:
 *   - Token reclassification
 *   - Table compression
 *   - Heavy testing of the mini-LR core
 *)


(* when true, print parse actions *)
let traceParse = false

(* when true, keep some statistics useful for performance evaluation *)
let accounting = true

(* when true, we call the user's keep() functions *)
let use_keep = true

(* when true, use the mini LR core *)
let use_mini_lr = true


(* NOTE: in some cases, more detailed comments can be found in
 * elkhound/glr.h, as these data structures mirror the ones
 * defined there *)


(* identifier for a symbol *)
type tSymbolId = int


(* link from one stack node to another *)
type tSiblingLink = {
  (* stack node we're pointing at; == cNULL_STACK_NODE if none *)
  mutable sib: tStackNode;

  (* semantic value on this link *)
  mutable sval: tSemanticValue;

  (* TODO: source location *)

  (* possible TODO: yield count *)
}

(* node in the GLR graph-structured stack; all fields are
 * mutable because these are stored in a pool for explicit re-use *)
and tStackNode = {
  (* for access to parser context in a few unusual situations *)
   glr: tGLR;

  (* LR parser state when this node is at the top *)
  mutable state: tStateId;

  (* pointers to adjacent (to the left) stack nodes *)
  (* possible TODO: put links into a pool so I can deallocate them *)
  mutable leftSiblings: tSiblingLink list;

  (* logically first sibling in the sibling list; separated out
   * from 'leftSiblings' for performance reasons *)
  mutable firstSib: tSiblingLink;

  (* number of sibling links pointing at this node, plus the
   * number of worklists this node appears in *)
  mutable referenceCount: int;

  (* number of links we can follow to the left before hitting a node
   * that has more than one sibling *)
  mutable determinDepth: int;

  (* position of token that was active when this node was created
   * (or pulled from pool); used in yield-then-merge calculations *)
  mutable column: int;
}

(* this is a path that has been queued for reduction;
 * all fields mutable to support pooling *)
and tPath = {
  (* rightmost state's id *)
  mutable startStateId: tStateId;

  (* production we're going to reduce with *)
  mutable prodIndex: int;

  (* column from leftmost stack node *)
  mutable startColumn: int;

  (* the leftmost stack node itself *)
  mutable leftEdgeNode: tStackNode;

  (* array of sibling links, i.e. the path; 0th element is
   * leftmost link *)
  sibLinks: tSiblingLink array ref;

  (* corresponding array of symbol ids to interpret svals *)
  symbols: tSymbolId array ref;

  (* next path in dequeueing order *)
  mutable next: tPath option;
}

(* priority queue of reduction paths *)
and tReductionPathQueue = {
  (* head of the list, first to dequeue *)
  mutable top: tPath option;

  (* pool of path objects *)
  pathPool: tPath Objpool.t;

  (* need our own copy of the tables pointer *)
  rpqTables: tParseTables;      (* name can't collide with tGLR.tables.. ! *)
}


(* GLR parser object *)
(* some mutable fields are for hack in 'makeGLR' *)
and tGLR = {
  (* user-specified actions *)
  userAct: tUserActions;

  (* parse tables from the grammar *)
  tables: tParseTables;

  (* set of topmost parser nodes *)
  mutable topmostParsers: tStackNode Arraystack.t;

  (* treat this as a local variable of rwlProcessWorklist, included
   * here just to avoid unnecessary repeated allocation *)
  toPass: tSemanticValue array;

  (* swapped with 'topmostParsers' periodically, for performance reasons *)
  mutable prevTopmost: tStackNode Arraystack.t;

  (* node allocation pool; shared with glrParse *)
  mutable stackNodePool: tStackNode Objpool.t;

  (* reduction queue and pool *)
  mutable pathQueue: tReductionPathQueue;

  (* when true, print some diagnosis of failed parses *)
  mutable noisyFailedParse: bool;

  (* current token number *)
  mutable globalNodeColumn: int;

  (* parser action statistics *)
  mutable detShift: int;
  mutable detReduce: int;
  mutable nondetShift: int;
  mutable nondetReduce: int;
}



(* what follows is based on elkhound/glr.cc *)


(* -------------------- misc constants --------------------- *)
(* maximum RHS length for mini-lr core *)
let cMAX_RHSLEN = 30

let cTYPICAL_MAX_REDUCTION_PATHS = 5

let cINITIAL_RHSLEN_SIZE = 10


(* ------------------ accounting statistics ----------------- *)
let numStackNodesAllocd: int ref = ref 0

let maxStackNodesAllocd: int ref = ref 0


(* ----------------- front ends to user code --------------- *)
let symbolDescription (sym: tSymbolId) (user: tUserActions)
                      (sval: tSemanticValue) : string =
begin
  if (symIsTerm sym) then (
    (user.terminalDescription (symAsTerm sym) sval)
  )
  else (
    (user.nonterminalDescription (symAsNonterm sym) sval)
  )
end


let duplicateSemanticValue (glr: tGLR) (sym: tSymbolId) (sval: tSemanticValue)
  : tSemanticValue =
begin
  (assert (sym <> 0));

  (* the C++ implementation checks for NULL sval, but I don't think
   * that can be here in the ML version, and I'm not convinced the
   * check would even be safe *)

  if (symIsTerm sym) then (
    (glr.userAct.duplicateTerminalValue (symAsTerm sym) sval)
  )
  else (
    (glr.userAct.duplicateNontermValue (symAsNonterm sym) sval)
  )
end


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

let makeSiblingLink s sv =
  { sib = s; sval = sv; }


(* --------------------- StackNode -------------------------- *)
(* NULL stack node *)
let cNULL_STACK_NODE = {
  state          = cSTATE_INVALID;
  leftSiblings   = [];
  firstSib       = (Obj.magic []);
  referenceCount = 0;
  determinDepth  = 0;
  glr            = (Obj.magic []);
  column         = 0
}


let emptyStackNode g =
  {
    state = cSTATE_INVALID;
    leftSiblings = [];
    firstSib = (makeSiblingLink cNULL_STACK_NODE cNULL_SVAL);
    referenceCount = 0;
    determinDepth = 0;
    glr = g;
    column = 0
  }


let getNodeSymbol (ths: tStackNode) : tSymbolId =
  ths.glr.tables.stateSymbol.(ths.state)


let incRefCt (ths: tStackNode) : unit =
  ths.referenceCount <- ths.referenceCount + 1


let rec decRefCt ths =
  assert (ths.referenceCount > 0);

  ths.referenceCount <- ths.referenceCount - 1;

  (*(Printf.printf "decrementing node %d to %d\n" ths.state ths.referenceCount);*)
  (*(flush stdout);*)

  if ths.referenceCount = 0 then (
    deinitStackNode ths;
    Objpool.dealloc ths.glr.stackNodePool ths
  )


and deinitStackNode ths =
  deallocSemanticValues ths;

  (* this is implicit in the C++ implementation because firstSib.sib
   * is an RCPtr in C++ *)
  (if (ths.firstSib.sib != cNULL_STACK_NODE) then (
    (decRefCt ths.firstSib.sib)
  ));

  ths.firstSib.sib <- cNULL_STACK_NODE;

  if (accounting) then (
    (decr numStackNodesAllocd);
    (*
      (Printf.printf "(...) deinit stack node: num=%d max=%d\n"
                     !numStackNodesAllocd
                     !maxStackNodesAllocd);
      (flush stdout);
    *)
  )


and deallocSemanticValues (ths: tStackNode) : unit =
  if (ths.firstSib.sib != cNULL_STACK_NODE) then (
    (deallocateSemanticValue (getNodeSymbol ths) ths.glr.userAct ths.firstSib.sval)
  );

  (List.iter
    (fun s -> (
      deallocateSemanticValue (getNodeSymbol ths) ths.glr.userAct s.sval;

      (* this is implicit in the C++ version, due to Owner<> *)
      decRefCt s.sib
    ))
    ths.leftSiblings);
  ths.leftSiblings <- []


let initStackNode (ths: tStackNode) (st: tStateId) : unit =
  ths.state <- st;
  (assert (isEmpty ths.leftSiblings));
  (assert (ths.firstSib.sib == cNULL_STACK_NODE));
  ths.referenceCount <- 0;
  ths.determinDepth <- 1;

  if (accounting) then (
    (incr numStackNodesAllocd);
    if (!numStackNodesAllocd > !maxStackNodesAllocd) then (
      maxStackNodesAllocd := !numStackNodesAllocd;
    );
    (*
      (Printf.printf "(!!!) init stack node: num=%d max=%d\n"
                     !numStackNodesAllocd
                     !maxStackNodesAllocd);
      (flush stdout);
    *)
  )


let hasZeroSiblings (ths: tStackNode) : bool =
begin
  (ths.firstSib.sib == cNULL_STACK_NODE)
end


let hasOneSibling (ths: tStackNode) : bool =
begin
  (ths.firstSib.sib != cNULL_STACK_NODE) && (isEmpty ths.leftSiblings)
end


let hasMultipleSiblings (ths: tStackNode) : bool =
begin
  not (isEmpty ths.leftSiblings)
end


let addFirstSiblingLink_noRefCt (ths: tStackNode) (leftSib: tStackNode)
                                (sval: tSemanticValue) : unit =
begin
  (assert (hasZeroSiblings ths));

  ths.determinDepth <- leftSib.determinDepth + 1;

  (assert (ths.firstSib.sib == cNULL_STACK_NODE));
  ths.firstSib.sib <- leftSib;     (* update w/o refct *)

  ths.firstSib.sval <- sval;
end


let addAdditionalSiblingLink (ths: tStackNode) (leftSib: tStackNode)
                             (sval: tSemanticValue) : tSiblingLink =
begin
  (* now there is a second outgoing pointer *)
  ths.determinDepth <- 0;

  (* this was implicit in the C++ verison *)
  (incRefCt leftSib);

  let link:tSiblingLink = (makeSiblingLink leftSib sval) in
  ths.leftSiblings <- link :: ths.leftSiblings;
  link
end


let addSiblingLink (ths: tStackNode) (leftSib: tStackNode)
                   (sval: tSemanticValue) : tSiblingLink =
begin
  if (ths.firstSib.sib == cNULL_STACK_NODE) then (
    (addFirstSiblingLink_noRefCt ths leftSib sval);

    (* manually inc refct *)
    (incRefCt leftSib);

    (* pointer to firstSib.. *)
    ths.firstSib
  )
  else (
    (addAdditionalSiblingLink ths leftSib sval)
  )
end


let getUniqueLink (ths: tStackNode) : tSiblingLink =
begin
  (assert (hasOneSibling ths));
  ths.firstSib
end


let getLinkTo (ths: tStackNode) (another: tStackNode)
  : tSiblingLink (*not tStackNode!*) option =
begin
  (* first? *)
  if (ths.firstSib.sib == another) then (
    (Some ths.firstSib)
  )

  else (
    (* rest? *)
    try
      let link:tSiblingLink = (List.find
        (fun candidate -> (candidate.sib == another))
        ths.leftSiblings) in
      (Some link)
    with Not_found ->
      None
  )
end


(* printAllocStats goes here *)

let computeDeterminDepth (ths: tStackNode) : int =
begin
  if (hasZeroSiblings ths) then (
    1
  )
  else if (hasOneSibling ths) then (
    (* sibling's plus one *)
    ths.firstSib.sib.determinDepth + 1
  )
  else (
    (assert (hasMultipleSiblings ths));
    0
  )
end


let checkLocalInvariants (ths: tStackNode) : bool =
begin
  (computeDeterminDepth ths) = ths.determinDepth;
end


(* ----------------------- stack node list ops ---------------------- *)
let decParserList lst=
  Arraystack.iter lst decRefCt


let incParserList lst =
  Arraystack.iter lst incRefCt


let parserListContains lst n =
  Arraystack.contains lst ((==) n)


(* ----------------------------- GLR --------------------------------- *)
let makePath() =
  {
    startStateId = cSTATE_INVALID;
    prodIndex = -1;
    startColumn = -1;
    leftEdgeNode = cNULL_STACK_NODE;
    sibLinks = ref (Array.make cINITIAL_RHSLEN_SIZE cNULL_SIBLING_LINK);
    symbols = ref (Array.make cINITIAL_RHSLEN_SIZE 0);
    next = None;
  }


let makeReductionPathQueue tablesIn =
  let allocator() : tPath = (makePath ()) in
  {
    top = None;
    pathPool = Objpool.make allocator;
    rpqTables = tablesIn;
  }


let makeGLR tablesIn actions =
  let glr:tGLR = {
    userAct = actions;
    tables = tablesIn;
    topmostParsers = ((Obj.magic []) : tStackNode Arraystack.t);  (* HACK!! *)
    toPass = (Array.make cMAX_RHSLEN cNULL_SVAL);
    prevTopmost = ((Obj.magic []) : tStackNode Arraystack.t);     (* HACK!! *)
    stackNodePool = ((Obj.magic []) : tStackNode Objpool.t);   (* HACK!! *)
    pathQueue = ((Obj.magic []) : tReductionPathQueue);          (* HACK!! *)
    noisyFailedParse = true;
    globalNodeColumn = 0;
    detShift = 0;
    detReduce = 0;
    nondetShift = 0;
    nondetReduce = 0
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
   * In fact, I *did* use the 'option' approach for tSiblingLink.sib,
   * and it is indeed a pain.
   * UPDATE: Switched to using Obj.magic there too, for performance.
   *)

  glr.topmostParsers <- (Arraystack.make cNULL_STACK_NODE);
  glr.prevTopmost <- (Arraystack.make cNULL_STACK_NODE);
  glr.stackNodePool <- (Objpool.make (fun () -> emptyStackNode glr));
  glr.pathQueue <- (makeReductionPathQueue tablesIn);

  if use_mini_lr then (
    (* check that none of the productions exceed cMAX_RHSLEN *)
    for i=0 to (glr.tables.numProds-1) do
      let len:int = (getProdInfo_rhsLen glr.tables i) in
      if (len > cMAX_RHSLEN) then (
        (* I miss token concatenation...*)
        (Printf.printf "Production %d contains %d right-hand side symbols,\nbut the GLR core has been compiled with a limit of %d.\nPlease adjust cMAX_RHSLEN and recompile the GLR core.\n"
                       i len cMAX_RHSLEN);
        (flush stdout);
        (failwith "cannot continue");
      );
    done;
  );

  glr


(* printConfig goes here *)

let rec grabTopSval (glr: tGLR) (node: tStackNode) : tSemanticValue =
begin
  let sib:tSiblingLink = (getUniqueLink node) in
  let ret:tSemanticValue = sib.sval in
  sib.sval <- (duplicateSemanticValue glr (getNodeSymbol node) sib.sval);

  (* TRSACTION("dup'd " << ret << " for top sval, yielded " << sib->sval); *)

  ret
end


(* macro in C++ version *)
let mMAKE_STACK_NODE (*dest is retval*) (state: tStateId) (glr: tGLR)
                     (pool: tStackNode Objpool.t) : tStackNode =
begin
  let dest:tStackNode = Objpool.alloc pool in
  (initStackNode dest state);
  dest.column <- glr.globalNodeColumn;
  dest
end

let makeStackNode (glr: tGLR) (state: tStateId) : tStackNode =
begin
  (mMAKE_STACK_NODE state glr glr.stackNodePool)
end


let addTopmostParser (glr: tGLR) (parsr: tStackNode) : unit =
begin
  (assert (checkLocalInvariants parsr));

  (Arraystack.push glr.topmostParsers parsr);
  (incRefCt parsr);

  (* no USE_PARSER_INDEX *)
end


(* stackTraceString *)


let initPath (ths: tPath) (ssi: tStateId) (pi: int) (rhsLen: int) : unit =
begin
  ths.startStateId <- ssi;
  ths.prodIndex <- pi;

  (* just use the 0th elements as the dummy 'null' value *)
  (Arraystack.ensureIndexDoubler ths.sibLinks rhsLen ((!(ths.sibLinks)).(0)));
  (Arraystack.ensureIndexDoubler ths.symbols rhsLen ((!(ths.symbols)).(0)));
end


let newPath (ths: tReductionPathQueue) (ssi: tStateId) (pi: int)
            (rhsLen: int) : tPath =
begin
  let p:tPath = Objpool.alloc ths.pathPool in
  (initPath p ssi pi rhsLen);
  p
end


let goesBefore (ths: tReductionPathQueue) (p1: tPath) (p2: tPath) : bool =
begin
  if (p1.startColumn > p2.startColumn) then (
    (* 'p1' spans fewer tokens, so it goes first *)
    true
  )
  else if (p2.startColumn > p1.startColumn) then (
    (* same logic *)
    false
  )
  else (
    (* equal start columns, compare nonterm ids *)
    let p1NtIndex:int = (getProdInfo_lhsIndex ths.rpqTables p1.prodIndex) in
    let p2NtIndex:int = (getProdInfo_lhsIndex ths.rpqTables p2.prodIndex) in

    (* check nonterm order *)
    let ord1:int = (getNontermOrdinal ths.rpqTables p1NtIndex) in
    let ord2:int = (getNontermOrdinal ths.rpqTables p2NtIndex) in

    ord1 < ord2
  )
end


let insertPathCopy (ths:tReductionPathQueue) (src: tPath)
                   (leftEdge: tStackNode) : unit =
begin
  let rhsLen:int = (getProdInfo_rhsLen ths.rpqTables src.prodIndex) in

  (* make a new node *)
  let p:tPath = Objpool.alloc ths.pathPool in
  (initPath p src.startStateId src.prodIndex rhsLen);

  (* fill in left edge info *)
  p.leftEdgeNode <- leftEdge;
  p.startColumn <- leftEdge.column;

  (* copy path info *)
  (Array.blit
    !(src.sibLinks)       (* source array *)
    0                     (* source start position *)
    !(p.sibLinks)         (* dest array *)
    0                     (* dest start position *)
    rhsLen                (* number of elements to copy *)
  );
  (Array.blit
    !(src.symbols)        (* source array *)
    0                     (* source start position *)
    !(p.symbols)          (* dest array *)
    0                     (* dest start position *)
    rhsLen                (* number of elements to copy *)
  );

  (* find proper place to insert new path *)
  if (not (isSome ths.top) || (goesBefore ths p (getSome ths.top))) then (
    (* prepend *)
    p.next <- ths.top;
    ths.top <- (Some p);
  )
  else (
    (* search *)
    let prev: tPath ref = ref (getSome ths.top) in
    while ((isSome !prev.next) && (not (goesBefore ths p (getSome !prev.next)))) do
      prev := (getSome !prev.next);
    done;

    (* insert *)
    p.next <- !prev.next;
    !prev.next <- (Some p);
  );
end


(* same argument meanings as for 'rwlRecursiveEnqueue' *)
let rec rwlCollectPathLink
  (glr: tGLR)
  (proto: tPath)
  (popsRemaining: int)
  (currentNode: tStackNode)
  (mustUseLink: tSiblingLink option)
  (linkToAdd: tSiblingLink)         (* extra *)
  : unit =
begin
  (!(proto.sibLinks)).(popsRemaining) <- linkToAdd;
  (!(proto.symbols)).(popsRemaining) <- (getNodeSymbol currentNode);

  if (someEquals mustUseLink linkToAdd) then (
    (* consume must-use link *)
    (rwlRecursiveEnqueue glr proto popsRemaining linkToAdd.sib
                         None(*mustUse*));
  )
  else (
    (rwlRecursiveEnqueue glr proto popsRemaining linkToAdd.sib
                         mustUseLink);
  );
end


(* recursive depth-first enumeration of paths *)
and rwlRecursiveEnqueue
  (glr: tGLR)
  (proto: tPath)                      (* prototype path, with path so far *)
  (popsRemaining: int)                (* # of links yet to traverse to find a full path *)
  (currentNode: tStackNode)           (* node we're at in the path *)
  (mustUseLink: tSiblingLink option)  (* link the path must use (if not None) *)
  : unit =
begin
  if (popsRemaining = 0) then (
    (* found path *)

    (* must have used the link *)
    match mustUseLink with
    | Some _ ->
        (* do nothing *)
        ()

    | None ->
        (* copy the prototype path, it's the one we want *)
        insertPathCopy glr.pathQueue proto currentNode
  )

  else (
    (* explore currentNode's siblings *)
    (rwlCollectPathLink glr proto (popsRemaining-1) currentNode mustUseLink
                        currentNode.firstSib);

    (List.iter
      (fun sibling -> (
        (rwlCollectPathLink glr proto (popsRemaining-1) currentNode mustUseLink
                            sibling)
      ))
      currentNode.leftSiblings
    );
  );
end


let deletePath (ths: tReductionPathQueue) (p: tPath) : unit =
begin
  Objpool.dealloc ths.pathPool p;
end


(* returns # of actions *)
let rec rwlEnqueueReductions (glr: tGLR) (parsr: tStackNode) (action: tActionEntry)
                         (mustUseLink: tSiblingLink option) : int =
begin
  (assert (checkLocalInvariants parsr));

  if (isShiftAction glr.tables action) then (
    (* do nothing, only looking for reductions *)
    1
  )
  else if (isReduceAction (*tables*) action) then (
    let prodIndex = (decodeReduce (*tables*) action parsr.state) in

    (* production info *)
    let rhsLen:int = (getProdInfo_rhsLen glr.tables prodIndex) in
    (assert (rhsLen >= 0));       (* paranoia *)

    (* make a prototype path; used to control recursion *)
    let proto:tPath =
      (newPath glr.pathQueue parsr.state prodIndex rhsLen) in

    (* kick off the recursion *)
    (rwlRecursiveEnqueue glr proto rhsLen parsr mustUseLink);

    (* deallocate prototype *)
    (deletePath glr.pathQueue proto);

    1
  )
  else if (isErrorAction (*tables*) action) then (
    (* parser just dies *)
    0
  )
  else (
    (* ambiguous; check for reductions in list of actions *)
    let firstEntry:int = (decodeAmbigAction glr.tables action parsr.state) in
    let numEntries:int = glr.tables.ambigTable.(firstEntry) in
    for i = 1 to numEntries do
      (* ignore return value because I know it will be 1 *)
      ignore (rwlEnqueueReductions glr parsr glr.tables.ambigTable.(firstEntry+i) mustUseLink);
    done;

    numEntries
  )
end


let queueIsNotEmpty (ths: tReductionPathQueue) : bool =
begin
  (isSome ths.top)
end


let dequeue (ths: tReductionPathQueue) : tPath =
begin
  let ret:tPath = (getSome ths.top) in
  ths.top <- ret.next;
  ret
end


let doReductionAction (glr: tGLR) (productionId: int)
                      (svals: tSemanticValue array) : tSemanticValue =
begin
  (glr.userAct.reductionAction productionId svals)
end


let findTopmostParser (glr: tGLR) (state: tStateId) : tStackNode option =
begin
  (* always using the *not* USE_PARSER_INDEX case *)
  (Arraystack.findOption glr.topmostParsers
    (fun n -> n.state = state))
end


let canMakeProgress tokType (glr: tGLR) (parsr: tStackNode) : bool =
  let entry = getActionEntry glr.tables parsr.state tokType in

  isShiftAction glr.tables entry ||
  isReduceAction (*tables*) entry ||
  not (isErrorAction (*tables*) entry)


(* ------------------------ RWL algorithm ------------------------- *)
let rwlShiftNonterminal tokType (glr: tGLR) (leftSibling: tStackNode) (lhsIndex: int)
                        (sval: tSemanticValue) : tSiblingLink option =
begin
  (* consult goto table to find where to go upon "shifting" the nonterminal *)
  let rightSiblingState:tStateId =
    (decodeGoto (getGotoEntry glr.tables leftSibling.state lhsIndex) lhsIndex) in

  if (traceParse) then (
    (Printf.printf "state %d, shift nonterm %d, to state %d\n"
                   leftSibling.state
                   lhsIndex
                   rightSiblingState);
    (flush stdout);
  );

  (* is there already an active parser with this state? *)
  match (findTopmostParser glr rightSiblingState) with
  | Some(rightSibling) -> (     (* rightSibling:tStackNode *)
      match (getLinkTo rightSibling leftSibling) with
      | Some(sibLink) -> (      (* sibLink:tSiblingLink *)
          (* we already have a sibling link, don't need a new one *)

          (* +--------------------------------------------------+
           * | it is here that we are bringing the tops of two  |
           * | alternative parses together (TREEBUILD)          |
           * +--------------------------------------------------+
           *)

          (* dead tree optimization *)
          if (not (canMakeProgress tokType glr rightSibling)) then (
            if (traceParse) then (
              (Printf.printf "avoided a merge by noticing the state was dead\n");
              (flush stdout);
            );
            (deallocateSemanticValue (getNodeSymbol rightSibling) glr.userAct sval);
          )
          else (
            (* call user's merge code *)
            sibLink.sval <-
              (glr.userAct.mergeAlternativeParses lhsIndex sibLink.sval sval);
          );

          (* ok, done *)
          None

          (* didn't add a link, no potential for new paths *)
        )

      | None -> (
          (* no suitable sibling link already, so add it *)
          let sibLink:tSiblingLink = (addSiblingLink rightSibling leftSibling sval) in

          (* recompute depths; TODO: do the topological sort thing *)
          if (rightSibling.referenceCount > 1) then (
            let changes: int ref = ref 1 in
            let iters: int ref = ref 0 in
            while (!changes > 0) do
              changes := 0;
              (Arraystack.iter glr.topmostParsers
                (fun parsr -> (
                  let newDepth:int = (computeDeterminDepth parsr) in
                  if (newDepth <> parsr.determinDepth) then (
                    (incr changes);
                    parsr.determinDepth <- newDepth;
                  );
                )));
              (incr iters);
              (assert (!iters < 1000));     (* protect against infinite loop *)
              (* computeDepthIters++; *)
            done;
          );

          (* inform caller of new link *)
          (Some sibLink)
        )
    )

  | None -> (
      (* not already active parser in this state, so make one *)
      let rightSibling:tStackNode = (makeStackNode glr rightSiblingState) in

      (* add link *)
      ignore (addSiblingLink rightSibling leftSibling sval);

      (* extend frontier *)
      (addTopmostParser glr rightSibling);

      (* enqueue this new parser's reductions *)
      let action:tActionEntry =
        (getActionEntry glr.tables rightSibling.state tokType) in
      ignore (rwlEnqueueReductions glr rightSibling action None(*siblink*));

      (* caller doesn't need to do anything more *)
      None
    )
end


let rwlProcessWorklist tokType (glr: tGLR) : unit =
begin
  while (queueIsNotEmpty glr.pathQueue) do
    (* process enabled reductions in priority order *)
    let path:tPath = (dequeue glr.pathQueue) in

    (* production info *)
    let rhsLen:int = (getProdInfo_rhsLen glr.tables path.prodIndex) in
    let lhsIndex:int = (getProdInfo_lhsIndex glr.tables path.prodIndex) in

    if (traceParse) then (
      (Printf.printf "state %d, reducing by production %d (rhsLen=%d), back to state %d\n"
                     path.startStateId
                     path.prodIndex
                     rhsLen
                     path.leftEdgeNode.state);
      (flush stdout);
    );

    if (accounting) then (
      glr.nondetReduce <- glr.nondetReduce + 1;
    );

    (* leftEdge *)

    (* before calling user's code, duplicate svals *)
    for i = (rhsLen-1) downto 0 do
      let sib:tSiblingLink = (!(path.sibLinks)).(i) in

      (* put the sval in the array that will be passed to the user *)
      glr.toPass.(i) <- sib.sval;

      (* source loc stuff *)

      (* ask user to duplicate, store that back in 'sib' *)
      sib.sval <- (duplicateSemanticValue glr (!(path.symbols)).(i) sib.sval);
    done;

    (* invoke user's reduction action (TREEBUILD) *)
    let sval:tSemanticValue =
      (doReductionAction glr path.prodIndex glr.toPass) in

    (* did user want to keep? *)
    if (use_keep &&
        (not (glr.userAct.keepNontermValue lhsIndex sval))) then (
      (* cancelled; drop on floor *)
    )
    else (
      (* shift the nonterminal, sval *)
      let newLink: tSiblingLink option =
        (rwlShiftNonterminal tokType glr path.leftEdgeNode lhsIndex sval) in

      if (isSome newLink) then (
        (* for each 'finished' parser, enqueue actions enabled by the new link *)
        (Arraystack.iter glr.topmostParsers
          (fun parsr ->
            let action:tActionEntry =
              (getActionEntry glr.tables parsr.state tokType) in
            ignore (rwlEnqueueReductions glr parsr action newLink)
          ));
      );
    );

    (* we dequeued it above, and are now done with it, so recycle
     * it for future use *)
    (deletePath glr.pathQueue path)
  done;
end


let rwlShiftTerminals tokenKindDesc (lexer : Lexerint.t) (glr: tGLR) : unit =
  glr.globalNodeColumn <- glr.globalNodeColumn + 1;

  (* move all parsers from 'topmostParsers' to 'prevTopmost' *)
  (assert (Arraystack.isEmpty glr.prevTopmost));
  (Arraystack.swapWith glr.prevTopmost glr.topmostParsers);
  (assert (Arraystack.isEmpty glr.topmostParsers));

  (* grab current token since we'll need it and the access
   * isn't all that fast here in ML *)
  let tokType = lexer.tokType in

  (* for token multi-yield.. *)
  let prev: tSiblingLink option ref = ref None in

  while not (Arraystack.isEmpty glr.prevTopmost) do
    (* take the node from 'prevTopmost'; the refcount transfers
     * from 'prevTopmost' to (local variable) 'leftSibling' *)
    let leftSibling:tStackNode = (Arraystack.pop glr.prevTopmost) in
    (assert (leftSibling.referenceCount >= 1));   (* for the local *)

    (* can this parser shift? *)
    let action:tActionEntry =
      getActionEntry glr.tables leftSibling.state tokType
    in

    (* if we find a shift, this will be set to something valid *)
    let newState: tStateId ref = ref cSTATE_INVALID in

    (* consult action table, looking for shifts *)
    if (isShiftAction glr.tables action) then (
      (* unambiguous shift *)
      newState := (decodeShift (*tables*) action tokType);
    )
    else if ((isReduceAction (*tables*) action) ||
             (isErrorAction (*tables*) action)) then (
      (* unambiguous reduction or error *)
    )
    else (
      (* nondeterministic *)
      let firstEntry:int = (decodeAmbigAction glr.tables action leftSibling.state) in
      let numEntries:int = glr.tables.ambigTable.(firstEntry) in

      for i=1 to numEntries do
        let action:tActionEntry = glr.tables.ambigTable.(firstEntry+i) in
        if (isShiftAction glr.tables action) then (
          (* a shift was among the conflicted actions *)
          newState := (decodeShift (*tables*) action tokType);

          (* would like to break out, but I can't, so just eat the wasted time.. *)
        );
      done;
    );

    if (!newState <> cSTATE_INVALID) then (
      (* found a shift *)

      if accounting then
        glr.nondetShift <- glr.nondetShift + 1;

      if (traceParse) then (
        (Printf.printf "state %d, shift token %s, to state %d\n"
                       leftSibling.state
                       (tokenKindDesc lexer.tokType)
                       !newState);
        (flush stdout);
      );

      (* already a parser in this state? *)
      let rightSibling =
        match findTopmostParser glr !newState with
        | Some rs -> rs     (* use existing *)
        | None -> (
            (* must make a new stack node *)
            let rs = makeStackNode glr !newState in

            (* add it to active parsers *)
            addTopmostParser glr rs;

            rs               (* use new *)
          )
      in

      (* semantic value for this token *)
      let sval =
        match !prev with
        | None -> Lexerint.(lexer.sval)    (* usual case *)

        | Some prev -> (
            (* the 'sval' we just grabbed has already been claimed by
             * 'prev.sval'; get a fresh one by duplicating the latter *)
            glr.userAct.duplicateTerminalValue tokType prev.sval
          )
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

    flush stdout;
  end


let nondeterministicParseToken tokenKindDesc lexer glr =
  let lastToDie = ref cSTATE_INVALID in

  (* seed the reduction worklist by analyzing the top nodes *)
  Arraystack.iter glr.topmostParsers (fun parsr ->
    let tt = lexer.tokType in
    let action = getActionEntry glr.tables parsr.state tt in
    let actions = rwlEnqueueReductions glr parsr action None(*sibLink*) in
    
    if actions = 0 then (
      if traceParse then (
        Printf.printf "parser in state %d died\n" parsr.state;
        flush stdout
      );
      lastToDie := parsr.state
    )
  );

  (* drop into worklist processing loop *)
  rwlProcessWorklist lexer.tokType glr;

  (* do all shifts last *)
  rwlShiftTerminals tokenKindDesc lexer glr;

  (* error? *)
  if Arraystack.isEmpty glr.topmostParsers then (
    printParseErrorMessage tokenKindDesc lexer.tokType glr !lastToDie;
    false
  ) else (
    true
  )


let cleanupAfterParse (glr: tGLR) (treeTop: tSemanticValue ref) : bool =
  if traceParse then (
    Printf.printf "Parse succeeded!\n";
    flush stdout;
  );

  if not (Arraystack.length glr.topmostParsers = 1) then (
    Printf.printf "parsing finished with %d active parsers!\n"
                  (Arraystack.length glr.topmostParsers);
    flush stdout;
    false     (* return *)
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

  try
    for i = 0 to last do
      if Arraystack.elt glr.topmostParsers i = parsr then (
        (* remove by swapping *)
        if i < last then (
          Arraystack.setElt glr.topmostParsers i (Arraystack.elt glr.topmostParsers last);
        );
        ignore (Arraystack.pop glr.topmostParsers);   (* removes a reference *)
        decRefCt parsr;                    (* so decrement *)
        raise Exit;      (* "break" *)
      )
    done
  with
  | Exit -> ()


(* dumpGSS goes here *)
(* dumpGSSEdge goes here *)


let nodeSummary node =
  string_of_int node.state ^
  "[" ^ string_of_int node.referenceCount ^ "]"


let rec innerStackSummary printed node =
  if List.exists ((=) node) !printed then (
    (* already printed *)
    "(rep:" ^ nodeSummary node ^ ")"
  )
  
  else (
    (* remember that we've now printed 'node' *)
    printed := node :: !printed;

    if node.firstSib.sib == cNULL_STACK_NODE then (
      (* no siblings *)
      nodeSummary node
    )

    else if (isEmpty node.leftSiblings) then (
      (* one sibling *)
      nodeSummary node ^ "-" ^
      innerStackSummary printed node.firstSib.sib
    )

    else (
      (* multiple siblings *)
      let tmp1 =       (* force order of eval *)
        nodeSummary node ^ "-(" ^
        innerStackSummary printed node.firstSib.sib
      in
      let tmp2 =
        List.fold_left (fun acc link -> 
          acc ^ "|" ^ innerStackSummary printed link.sib
        ) "" node.leftSiblings
      in
      tmp1 ^ tmp2 ^ ")"
    )
  )


let stackSummary glr =
  (* nodes already printed *)
  let printed = ref [] in

  (* loop/fold *)
  let len = Arraystack.length glr.topmostParsers in
  let rec loop acc i =
    if i > len - 1 then
      acc                      (* done *)
    else
      let n = Arraystack.elt glr.topmostParsers i in

      loop
        (acc ^
         " (" ^ string_of_int i ^ ": " ^
         innerStackSummary printed n ^ ")")
        (i + 1)
  in

  loop "" 0


(* buildParserIndex goes here *)

let glrParse glr tokenKindDesc getToken lexer treeTop =
  (* make some things into local variables *)
  let userAct = glr.userAct in
  let tables = glr.tables in
  let topmostParsers = glr.topmostParsers in

  (*nextToken*)

  (*reclassifier*)

  (* the C++ implementation is careful to allocate the main pointer on
   * the stack to save an indirection; but I can't do that in the ML
   * version, so may as well just let the GLR object own the pool *)
  let stackNodePool = glr.stackNodePool in

  glr.globalNodeColumn <- 0;
  begin
    let first = makeStackNode glr 0(*startState*) in
    addTopmostParser glr first;
  end;

  (* array for passing semantic values in the mini lr core *)
  let toPass = Array.make cMAX_RHSLEN cNULL_SVAL in

  let localDetShift = ref 0 in
  let localDetReduce = ref 0 in

  (* pulled out so I can use this block of statements in several places *)
  let glrParseToken lexer =
    if not (nondeterministicParseToken tokenKindDesc lexer glr) then
      raise Exit;              (* "return false" *)

    (* goto label: getNextToken *)
    (* last token? *)
    if lexer.tokType = 0 then
      raise End_of_file;       (* "break" *)

    (* get the next token *)
    getToken lexer
  in

  (* main parsing loop *)
  try
    let rec main_loop lexer jump_to_mini_lr = (
      if not jump_to_mini_lr then (
        if traceParse then (
          Printf.printf "---- processing token %s, %d active parsers ----\n"
                         (tokenKindDesc lexer.tokType)
                         (Arraystack.length glr.topmostParsers);
          Printf.printf "Stack:%s\n" (stackSummary glr);
          flush stdout
        )

        (* reclassifyToken *)
      );

      (* ------------ glr parser ------------ *)
      (* --------- mini-lr parser ------- *)
      (* see elkhound/glr.cc for more details *)
      (* goto label: tryDeterministic *)
      if use_mini_lr && Arraystack.length topmostParsers = 1 then (
        let parsr: tStackNode ref = ref (Arraystack.top topmostParsers) in
        assert (!parsr.referenceCount = 1);

        let tok = lexer.tokType in
        let action = getActionEntry_noError tables !parsr.state tok in

        if isReduceAction action then (
          if accounting then (
            incr localDetReduce;
          );
          let prodIndex = decodeReduce action !parsr.state in
          let rhsLen = getProdInfo_rhsLen tables prodIndex in
          if rhsLen <= !parsr.determinDepth then (
            (* can reduce unambiguously *)
            let lhsIndex = getProdInfo_lhsIndex tables prodIndex in

            let startStateId = !parsr.state in

            assert (rhsLen <= cMAX_RHSLEN);

            (* loop for arbitrary rhsLen *)
            for i = rhsLen - 1 downto 0 do
              (* grab the (only) sibling of 'parsr' *)
              let sib = !parsr.firstSib in

              (* store its semantic value in the 'toPass' array *)
              toPass.(i) <- sib.sval;

              (* pop 'parsr' and move to next one *)
              Objpool.dealloc stackNodePool !parsr;
              let prev:tStackNode = !parsr in
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
              userAct.reductionAction prodIndex toPass
            in

            (* now, do an abbreviated 'glrShiftNonterminal' *)
            let newState =
              decodeGoto (getGotoEntry tables !parsr.state lhsIndex) lhsIndex
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
              mMAKE_STACK_NODE newState glr stackNodePool
            in

            addFirstSiblingLink_noRefCt newNode !parsr sval;

            assert (!parsr.referenceCount = 1);

            (* replace old topmost parser with 'newNode' *)
            Arraystack.setElt topmostParsers 0 newNode;
            incRefCt newNode;
            assert (newNode.referenceCount = 1);

            (* does the user want to keep it? *)
            if use_keep && not (userAct.keepNontermValue lhsIndex sval) then (
              printParseErrorMessage tokenKindDesc lexer.tokType glr newNode.state;
              if accounting then (
                glr.detShift <- glr.detShift + !localDetShift;
                glr.detReduce <- glr.detReduce + !localDetReduce;
              );

              raise Exit               (* "return false" *)
            );

            (* we have not shifted a token, so again try to use
             * the deterministic core *)
            main_loop lexer true(*jump*)     (* "goto tryDeterministic;" *)
          )

          else (
            (* deterministic depth insufficient: use GLR *)
            main_loop (glrParseToken lexer) false            (* tail call *)
          )
        )

        else if isShiftAction tables action then (
          if accounting then (
            incr localDetShift;
          );

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

          let rightSibling:tStackNode =
            mMAKE_STACK_NODE newState glr stackNodePool
          in

          addFirstSiblingLink_noRefCt rightSibling !parsr Lexerint.(lexer.sval);

          (* replace 'parsr' with 'rightSibling' *)
          Arraystack.setElt topmostParsers 0 rightSibling;

          assert (!parsr.referenceCount = 1);
          assert (rightSibling.referenceCount = 0);

          rightSibling.referenceCount <- 1;

          (* get next token *)
          (* "goto getNextToken;" *)
          (* last token? *)
          if lexer.tokType = 0 then
            raise End_of_file;       (* "break" *)

          (* get the next token *)
          main_loop (getToken lexer) false      (* tail call *)
        )

        else (
          (* error or ambig; not deterministic *)
          main_loop (glrParseToken lexer) false      (* tail call *)
        )
      )

      else (
        (* mini lr core disabled, use full GLR *)
        main_loop (glrParseToken lexer) false        (* tail call *)
      )
    ) in

    main_loop (getToken lexer) false

  with
  | Exit ->
      false

  | End_of_file ->
      if accounting then (
        glr.detShift <- glr.detShift + !localDetShift;
        glr.detReduce <- glr.detReduce + !localDetReduce;
      );

      (* end of parse *)
      cleanupAfterParse glr treeTop
