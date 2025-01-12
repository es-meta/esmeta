package esmeta.web

import esmeta.cfg.*
import esmeta.es.Ast
import esmeta.ir.{Func as IRFunc, *}
import esmeta.interpreter.Interpreter
import esmeta.lang.Syntax
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.state.*
import esmeta.util.Loc

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger extension of IR interpreter */
class Debugger(st: State) extends Interpreter(st, log = true) {
  // ---------------------------------------------------------------------------
  // shortcuts
  // ---------------------------------------------------------------------------
  private inline def cfg = st.cfg
  private inline def cursor = st.context.cursor
  private inline def func = cursor.func
  private inline def irFunc = func.irFunc
  private inline def algoOpt: Option[Algorithm] = irFunc.algo

  // ---------------------------------------------------------------------------
  // overrides IR interpreter
  // ---------------------------------------------------------------------------
  // transition for node to more fine-grained execution within block node
  override def step: Boolean = {
    val ret = super.step
    cursor match {
      case _: ExitCursor if st.callStack.nonEmpty =>
        triggerBreaks
        saveBpCounts
      case _: ExitCursor =>
      case _: NodeCursor =>
    }
    ret
  }

  override def eval(node: Node): Unit = {
    saveBpCounts // save counter
    (cursor, node) match
      case (cursor: NodeCursor, block @ Block(_, insts, next)) =>
        eval(insts(cursor.idx))
        cursor.idx += 1
        if (cursor.idx == insts.length) {
          cursor.idx -= 1
          st.context.moveNode
        }
      case _ => super.eval(node)
    triggerBreaks // trigger breakpoints
  }

  override def eval(expr: Expr): Value = expr match {
    case _: AllocExpr =>
      val addr = super.eval(expr)
      provenance._2 match {
        case Some(provAddr) if provAddr == addr => provenance = (getIter, None)
        case _                                  =>
      }
      addr
    case _ => super.eval(expr)
  }

  // ---------------------------------------------------------------------------
  // execution control
  // ---------------------------------------------------------------------------

  /** ToDo : eliminate unnecessary mutable variables */
  private var wasExited: Boolean = false
  private var provenance: (Int, Option[Addr]) = (0, None)

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed, ReachedFront

  // step until given predicate
  // TODO handle yet
  @tailrec
  final def stepWhile(
    pred: => Boolean,
    ignoreBreak: Boolean = false,
  ): StepResult =
    val (prevLoc, prevStackSize) = getIrInfo
    val keep = step
    val (curLoc, curStackSize) = getIrInfo

    wasExited =
      prevStackSize > curStackSize || (wasExited && prevLoc._2 == curLoc._2)
    val break = isBreaked && !ignoreBreak

    if (pred && keep && !break) stepWhile(pred, ignoreBreak)
    else if (break)
      StepResult.Breaked
    else if (keep)
      StepResult.Succeed
    else
      StepResult.Terminated

  def stepExactly(
    count: Int,
    ignoreBreak: Boolean = false,
    fn: Option[() => Unit] = None,
  ): StepResult = {
    // XXX should throw exception if count is negative?
    if count <= 0 then
      return if getIter == 0 then StepResult.ReachedFront
      else StepResult.Succeed
    stepWhile(
      {
        fn.map(_())
        val current = getIter
        current < count
      },
      ignoreBreak,
    )
  }

  private def stepExactlyFrom(
    to: Int,
    ignoreBreak: Boolean = false,
    from: Option[Int] = None,
    fn: Option[() => Unit] = None,
  ): StepResult =
    reset
    from.foreach(stepExactly(_, ignoreBreak))
    stepExactly(to, ignoreBreak, fn)

  private def reset: Unit =
    val newDOpt = for {
      sourceText <- st.sourceText
    } yield Debugger(cfg.init.from(sourceText))
    val newD = newDOpt.get
    this.st.context = newD.st.context
    this.st.callStack = newD.st.callStack
    this.st.heap.map.clear()
    this.st.heap.map ++= newD.st.heap.map
    this.st.heap.size = newD.st.heap.size
    this.st.globals.clear()
    this.st.globals ++= newD.st.globals
    setIter(0)

  private def getIrInfo = ((irFunc.name, cursor.stepsOpt), st.callStack.size)

  // spec step
  final def specStep(ignoreBreak: Boolean = false) = {
    val (prevLoc, _) = getIrInfo
    stepWhile(
      { prevLoc._2.isDefined && prevLoc == getIrInfo._1 },
      ignoreBreak,
    )
  }

  // spec step over
  final def specStepOver(ignoreBreak: Boolean = false) =
    val (prevLoc, prevStackSize) = getIrInfo
    stepWhile(
      {
        val (loc, stackSize) = getIrInfo
        (prevLoc._2.isDefined && prevLoc == loc) || (prevStackSize < stackSize)
      },
      ignoreBreak,
    )

  // spec step out
  final def specStepOut(ignoreBreak: Boolean = false) =
    val (_, prevStackSize) = getIrInfo
    stepWhile({ prevStackSize <= getIrInfo._2 }, ignoreBreak)

  // spec step back
  final def specStepBack(ignoreBreak: Boolean = false) = {
    val ((curLoc, curStackSize), curIter) = (getIrInfo, getIter)

    var target: Int = 0
    var breakFlag: Boolean = false

    val calcTarget = () => {
      val (iterLoc, iterStackSize) = getIrInfo
      val iterCond = curLoc._2.isDefined && curLoc == iterLoc
      if (!iterCond) {
        target = getIter
        breakpoints.foreach {
          case SpecBreakpoint(fid, steps, true)
              if st.context.func.id == fid && iterLoc._2.contains(
                steps,
              ) =>
            breakFlag = true
          case _ => breakFlag = false
        }
      }
    }

    stepExactlyFrom(curIter - 1, true, fn = Some(calcTarget))
    val result = stepExactlyFrom(target, true)
    if (breakFlag && !ignoreBreak) StepResult.Breaked else result
  }

  // spec step back over
  final def specStepBackOver(ignoreBreak: Boolean = false) = {
    val (curLoc, curStackSize) = getIrInfo
    val currentIter = getIter

    var target: Int = 0
    val calcTarget = () => {
      val (iterLoc, iterStackSize) = getIrInfo
      val cond =
        (curLoc._2.isDefined && curLoc == iterLoc) || (curStackSize < iterStackSize)
      if !cond then target = getIter
    }

    var breakFlag: Boolean = false
    val calcBp = () => {
      val (iterLoc, iterStackSize) = getIrInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(fid, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.id == fid && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = getIter
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcTarget))
    if (!ignoreBreak)
      stepExactlyFrom(
        currentIter - 1,
        true,
        from = Some(target),
        fn = Some(calcBp),
      )
    val result = stepExactlyFrom(target, true)
    if (breakFlag) StepResult.Breaked else result
  }

  // spec step back out
  final def specStepBackOut(ignoreBreak: Boolean = false) = {
    val (curLoc, curStackSize) = getIrInfo
    val currentIter = getIter

    var target: Int = 0
    var breakFlag: Boolean = false

    val calcTarget = () => {
      if !(curStackSize <= getIrInfo._2) then target = getIter
    }

    val calcBp = () => {
      val (iterLoc, iterStackSize) = getIrInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(fid, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.id == fid && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = getIter
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcTarget))
    if (!ignoreBreak)
      stepExactlyFrom(
        currentIter - 1,
        true,
        from = Some(target),
        fn = Some(calcBp),
      )
    val result = stepExactlyFrom(target, true)
    if (breakFlag) StepResult.Breaked else result
  }

  // es steps from ast span info
  private def getEsInfo =
    val ctxts = st.context :: st.callStack.map(_.context)
    val callStackSize = ctxts.count(_.isEsCall)
    val (ls, le) = ctxts.flatMap(_.esLocOpt).headOption.getOrElse((-1, -1))
    ((ls, le), callStackSize)

  // es step
  final def esStep =
    val (prevLoc, _) = getEsInfo
    stepWhile {
      val (loc, _) = getEsInfo
      loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1
    }

  // es step over
  final def esStepOver =
    val (prevLoc, prevStackSize) = getEsInfo
    stepWhile {
      val (loc, stackSize) = getEsInfo
      loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1 || prevStackSize < stackSize
    }

  // es step out
  final def esStepOut =
    val (_, prevStackSize) = getEsInfo
    stepWhile {
      val (loc, stackSize) = getEsInfo
      loc._1 == -1 || prevStackSize <= stackSize
    }

  // continue
  final def continue: StepResult = stepWhile { true }

  // rewind
  final def rewind: StepResult = {
    val (curLoc, curStackSize) = getIrInfo
    val currentIter = getIter
    var target: Int = 0
    var breakFlag: Boolean = false

    val calcBp = () => {
      val (iterLoc, iterStackSize) = getIrInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(fid, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.id == fid && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = getIter
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcBp))
    if (breakFlag)
      stepExactlyFrom(target, true)
      StepResult.Breaked
    else
      reset
      StepResult.Succeed
  }

  final def stepBackToProvenance(addr: Addr): StepResult = {
    provenance = (0, Some(addr))
    stepExactlyFrom(getIter, true)
    stepExactlyFrom(provenance._1, true)
  }

  // ---------------------------------------------------------------------------
  // breakpoints
  // ---------------------------------------------------------------------------

  /** breakpoints used in debugger */
  trait Breakpoint {
    var enabled: Boolean
    private var prevCount = 0
    private var trigger = false
    def needTrigger: Boolean = {
      if (trigger) { trigger = false; true }
      else false
    }
    protected def on: Unit = trigger = true
    protected def count: Int
    def saveCount: Unit = prevCount = count
    def check: Unit = if (enabled && prevCount < count) this.on
    def toggle() = { enabled = !enabled }
    protected def getContexts: List[Context] =
      st.context :: st.callStack.map(_.context)
  }

  /** breakpoints by spec steps */
  case class SpecBreakpoint(
    fid: Int,
    steps: List[Int],
    var enabled: Boolean = true,
  ) extends Breakpoint {
    override protected def count: Int =
      getContexts.count(ctxt =>
        ctxt.func.id == fid && ctxt.cursor.stepsOpt == Some(steps),
      )
  }

  /** breakpoints by ECMAScript code line */
  case class EsBreakpoint(
    line: Int,
    var enabled: Boolean = true,
  ) extends Breakpoint {
    @tailrec
    private def isParent(ast: Ast, target: Ast): Boolean = ast.parent match
      case Some(parent) =>
        if (parent eq target) true
        else isParent(parent, target)
      case None => false

    // count the number of breakpoint target ast
    override protected def count: Int =
      val astList = getContexts.flatMap { ctxt =>
        ctxt.astOpt match
          case Some(ast) if ast.loc.isDefined =>
            val loc = ast.loc.get
            val (ls, le) = (loc.start.line, loc.end.line)
            if (ls != -1 && ls == le && ls == line) Some(ast)
            else None
          case _ => None
      }
      astList.filter(ast => astList.exists(isParent(_, ast))).length
  }

  // breakpoints
  private val breakpoints: ListBuffer[Breakpoint] = ListBuffer()
  private def disableBps(): List[Int] = {
    val enabledIndices = breakpoints.zipWithIndex.collect {
      case (bp, idx) if bp.enabled => idx
    }.toList
    breakpoints.foreach(_.enabled = false)
    enabledIndices
  }
  private def enableBps(enableIndices: List[Int]): Unit = {
    enableIndices.foreach(idx => breakpoints(idx).enabled = true)
  }
  protected def ignoreBps[T](action: => T): T = {
    val enabledIndices = disableBps()
    val result = action
    enableBps(enabledIndices)
    result
  }

  // trigger breakpoints
  private def triggerBreaks: Unit =
    for (b <- breakpoints) b.check

  // save counter for each breakpoint
  private def saveBpCounts: Unit =
    for (b <- breakpoints) b.saveCount

  // add breakpoints from serialized data
  final def addBreak(
    data: (Boolean, Int, List[Int], Boolean),
  ): Unit =
    val (isEsBreak, num, steps, enabled) = data
    val bp =
      if (isEsBreak) EsBreakpoint(num, enabled)
      else SpecBreakpoint(num, steps, enabled)
    breakpoints += bp

  // remove breakpoint
  final def rmBreakAll: Unit = breakpoints.clear
  final def rmBreak(idx: Int): Unit = breakpoints.remove(idx)

  // toggle breakpoints
  final def toggleBreakAll: Unit = for { bp <- breakpoints } bp.toggle()
  final def toggleBreak(idx: Int): Unit = breakpoints(idx).toggle()

  // check if current step is in break
  private def isBreaked: Boolean = breakpoints.foldLeft(false) {
    case (acc, bp) => bp.needTrigger || acc
  }

  // ---------------------------------------------------------------------------
  // debugger info
  // ---------------------------------------------------------------------------

  extension (node: Node) {

    /** get location in spec of node */
    def stepsOpt: List[List[Int]] =

      def instList: List[Inst] = node match
        case Block(_, insts, _) => insts.toList
        case node: NodeWithInst => node.inst.fold(Nil)(_ :: Nil)
      def langOpt: List[Syntax] = instList.flatMap(_.langOpt)
      for {
        lang <- langOpt
        loc <- lang.loc
      } yield loc.steps

  }

  /** extension for cursor */
  extension (cursor: Cursor) {

    /** get ir instruction of current cursor */
    def instOpt: Option[Inst] = cursor match
      case NodeCursor(_, node, idx) =>
        node match
          case Block(_, insts, _) => Some(insts(idx))
          case node: NodeWithInst => node.inst
      case _: ExitCursor => None

    /** get syntax object of current cursor */
    def langOpt = cursor.instOpt.fold(None)(_.langOpt)

    /** get location in spec of current cursor */
    def stepsOpt: Option[List[Int]] = cursor.langOpt match
      case Some(lang) if lang.loc.isDefined => Some(lang.loc.get.steps)
      case _                                => None
  }

  /** extension for context */
  extension (ctxt: Context) {

    /** ast of current context */
    def astOpt: Option[Ast] =
      if (ctxt.func.isRuntimeSDO) Some(ctxt.locals(NAME_THIS).asAst)
      else None

    /** check if es call */
    def isEsCall: Boolean = ctxt.name == "Call" || ctxt.name == "Construct"

    /** location of ECMAScript code */
    def esLocOpt: Option[(Int, Int)] = astOpt match
      case Some(ast) =>
        Some(ast.loc match
          case Some(loc) => (loc.start.line, loc.end.line)
          case None      => (-1, -1),
        )
      case None => None
  }

  /** extension for func */
  extension (func: Func) {

    /** check whether it is runtime SDO */
    def isRuntimeSDO: Boolean = func.irFunc.head match
      case Some(SyntaxDirectedOperationHead(_, _, false, _, _)) => true
      case _                                                    => false
  }

  /** heap information */
  def heapInfo = st.heap.map.map {
    case (addr, obj) =>
      import io.circe.*
      import io.circe.syntax.*
      (
        addr.toString,
        obj match
          case RecordObj(tname, map) =>
            Json.obj(
              "type" -> Json.fromString("RecordObj"),
              "tname" -> Json.fromString(tname),
              "map" -> Json.fromFields(map.map {
                case (k, v) => (k, v.toString.asJson)
              }),
              "stringform" -> Json.fromString(obj.toString),
            )
          case MapObj(map) =>
            Json.obj(
              "type" -> Json.fromString("MapObj"),
              "map" -> Json.fromFields(map.map {
                case (k, v) => (k.toString, v.toString.asJson)
              }),
              "stringform" -> Json.fromString(obj.toString),
            )
          case ListObj(values) =>
            Json.obj(
              "type" -> Json.fromString("ListObj"),
              "values" -> Json.arr((values.map(_.toString.asJson))*),
              "stringform" -> Json.fromString(obj.toString),
            )
          case YetObj(tname, msg) =>
            Json.obj(
              "type" -> Json.fromString("YetObj"),
              "tname" -> Json.fromString(tname),
              "msg" -> Json.fromString(msg),
              "stringform" -> Json.fromString(obj.toString),
            ),
      )
  }

  /** call stack information */
  def callStackInfo =
    def getInfo(c: Context, wasExited: Boolean = false) =
      (
        c.func.id,
        c.name,
        c.cursor.stepsOpt.getOrElse(List()),
        wasExited,
        c.locals.collect {
          case (Name(name), v) => (name, v.toString)
        }.toList ++ c.retVal.map { case (_, v) => ("return", v.toString) }, {
          val dynamic = c.visited
          val static = cfg.depGraph.deps(cfg.funcMap(c.func.id))
          // what to do?

          val currentNode = c.cursor match
            case NodeCursor(_, node, _) => Some(node)
            case ExitCursor(_)          => None

          val intersection = currentNode match
            case Some(node) => dynamic intersect static.getOrElse(node, Set())
            case None       => dynamic

          (intersection ++ currentNode).flatMap(_.stepsOpt).toList
        },
        // st.cfg.depGraph.getStringForm()
      )

    getInfo(st.context, wasExited) :: st.callStack
      .map(_.context)
      .map(getInfo(_))

  /** context information */
  def ctxtInfo(cid: Int) =
    def paramInfo(p: Param) = (
      p.lhs.name,
      p.optional,
      p.ty.toString,
    )

    val ctxts = (st.context :: st.callStack.map(_.context)).drop(cid)
    val (os, oe) = ctxts.flatMap(_.astOpt) match
      case curr :: _ if curr.loc.isDefined =>
        val loc = curr.loc.get
        (loc.start.offset, loc.end.offset)
      case _ => (-1, -1)
    val ctxt = ctxts.head
    val (nid, isExit) = ctxt.cursor match
      case NodeCursor(_, n, _) => (n.id, false)
      case _: ExitCursor       => (-1, true)
    val func = ctxt.func
    val irFunc = func.irFunc
    val code = irFunc.algo.map(_.code).getOrElse("")
    (
      func.id,
      irFunc.kind.ordinal,
      irFunc.name,
      irFunc.params.map(paramInfo(_)),
      func.toDot(nid, isExit),
      code,
      (os, oe),
    )
}
