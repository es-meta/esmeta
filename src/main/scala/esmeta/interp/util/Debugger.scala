package esmeta.interp.util

import esmeta.LOG
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.Ast
import esmeta.lang.Syntax
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.util.Loc
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger extension of ir interpreter */
class Debugger(st: State) extends Interp(st, Nil) {
  LOG = true

  // ------------------------------------------------------------------------------
  // shortcuts
  // ------------------------------------------------------------------------------
  private inline def cfg = st.cfg
  private inline def cursor = st.context.cursor
  private inline def func = cursor.func
  private inline def irFunc = func.irFunc
  private inline def algoOpt: Option[Algorithm] = irFunc.algo

  // ------------------------------------------------------------------------------
  // overrides interpreter
  // ------------------------------------------------------------------------------
  // transition for node to more fine-grained execution within block node
  override def step: Boolean = (super.step, cursor) match
    // TODO handle call instruction
    case (_, _: ExitCursor) if !st.callStack.isEmpty => {
      val res = step; triggerBreaks; res
    }
    case (res, _) => res
  override def interp(node: Node): Unit = {
    node match
      case block @ Block(_, insts, next) if cursor.stepsOpt.isDefined =>
        interp(insts(cursor.idx))
        cursor.idx += 1
        if (cursor.idx == insts.length) st.context.moveNext
      case _ =>
        super.interp(node)
    triggerBreaks // trigger breakpoints
  }

  // ------------------------------------------------------------------------------
  // execution control
  // ------------------------------------------------------------------------------

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed

  // step until given predicate
  // TODO handle yet
  @tailrec
  final def stepUntil(pred: => Boolean): StepResult =
    if (!isBreaked) {
      val keep = step
      if (pred && keep) stepUntil(pred)
      else if (keep) StepResult.Succeed
      else StepResult.Terminated
    } else StepResult.Breaked

  private def getIrInfo = ((irFunc.name, cursor.stepsOpt), st.callStack.size)

  // spec step
  final def specStep = {
    val (prevLoc, _) = getIrInfo
    stepUntil { prevLoc._2.isDefined && prevLoc == getIrInfo._1 }
  }

  // spec step over
  final def specStepOver =
    val (prevLoc, prevStackSize) = getIrInfo
    stepUntil {
      val (loc, stackSize) = getIrInfo
      (prevLoc._2.isDefined && prevLoc == loc) || (prevStackSize < stackSize)
    }

  // spec step out
  final def specStepOut =
    val (_, prevStackSize) = getIrInfo
    stepUntil { prevStackSize <= getIrInfo._2 }

  // js steps from ast span info
  private def getJsInfo =
    val ctxts = st.context :: st.callStack.map(_.context)
    val callStackSize = ctxts.count(_.isJsCall)
    val (ls, le) = ctxts.flatMap(_.astOpt) match
      case curr :: _ if curr.loc.isDefined =>
        val loc = curr.loc.get
        (loc.start.line, loc.end.line)
      case _ => (-1, -1)
    ((ls, le), callStackSize)

  // js step
  final def jsStep =
    val (prevLoc, _) = getJsInfo
    stepUntil {
      val (loc, _) = getJsInfo
      println((prevLoc, loc))
      (loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1)
    }

  // js step over
  final def jsStepOver =
    val (prevLoc, prevStackSize) = getJsInfo
    stepUntil {
      val (loc, stackSize) = getJsInfo
      (loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1 || prevStackSize < stackSize)
    }

  // js step out
  final def jsStepOut =
    val (_, prevStackSize) = getJsInfo
    stepUntil {
      val (loc, stackSize) = getJsInfo
      loc._1 == -1 || prevStackSize <= stackSize
    }

  // continue
  final def continue: StepResult = stepUntil { true }

  // ------------------------------------------------------------------------------
  // breakpoints
  // ------------------------------------------------------------------------------

  /** breakpoints used in debugger */
  trait Breakpoint {
    var enabled: Boolean
    private var trigger = false
    def needTrigger: Boolean = {
      if (trigger) { trigger = false; true }
      else false
    }
    protected def on: Unit = trigger = true
    def check(st: State): Unit
    def toggle() = { enabled = !enabled }
  }

  /** breakpoints by spec steps */
  case class SpecBreakpoint(
    fid: Int,
    steps: List[Int],
    var enabled: Boolean = true,
  ) extends Breakpoint {
    override def check(st: State): Unit =
      if (enabled) {
        val currStepsOpt = st.context.cursor.stepsOpt
        if (st.func.id == fid && currStepsOpt == Some(steps))
          if (st.context.prevCursorOpt.fold(true)(_.stepsOpt != currStepsOpt))
            this.on
      }
  }

  // TODO breakpoints by JavaScript code line */
  case class JsBreakpoint(
    line: Int,
    var enabled: Boolean = true,
  ) extends Breakpoint {
    override def check(st: State): Unit = ???
  }

  // breakpoints
  private val breakpoints: ListBuffer[Breakpoint] = ListBuffer()

  // trigger breakpoints
  private def triggerBreaks: Unit = for (b <- breakpoints) b.check(st)

  // add breakpoints from serialized data
  final def addBreak(
    data: (Boolean, Int, List[Int], Boolean),
  ): Unit =
    val (isJsBreak, num, steps, enabled) = data
    val bp =
      if (isJsBreak) JsBreakpoint(num, enabled)
      else SpecBreakpoint(num, steps, enabled)
    breakpoints += bp

  // remove breakpoint
  final def rmBreakAll: Unit = breakpoints.clear
  final def rmBreak(idx: Int): Unit = breakpoints.remove(idx.toInt)

  // toggle breakpoints
  final def toggleBreakAll: Unit = for { bp <- breakpoints } bp.toggle()
  final def toggleBreak(idx: Int): Unit = breakpoints(idx).toggle()

  // check if current step is in break
  private def isBreaked: Boolean = breakpoints.foldLeft(false) {
    case (acc, bp) => bp.needTrigger || acc
  }

  // ------------------------------------------------------------------------------
  // debugger info
  // ------------------------------------------------------------------------------

  /** extension for cursor */
  extension (cursor: Cursor) {

    /** get cfg function of current cursor */
    def func = cursor match
      case NodeCursor(node) => cfg.funcOf(node)
      case ExitCursor(func) => func

    /** get ir instruction of current cursor */
    def instOpt: Option[Inst] = cursor match
      case NodeCursor(node) =>
        node match
          case Block(_, insts, _) => Some(insts(cursor.idx))
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

    /** check if js call */
    def isJsCall: Boolean = ctxt.name == "Call" || ctxt.name == "Construct"
  }

  /** extension for func */
  extension (func: Func) {

    /** check wheter it is runtime SDO */
    def isRuntimeSDO: Boolean = func.irFunc.head match
      case Some(SyntaxDirectedOperationHead(_, _, false, _, _)) => true
      case _                                                    => false
  }

  /** heap information */
  def heapInfo = st.heap.map.map {
    case (addr, obj) => (addr.toString, obj.toString)
  }

  /** call stack information */
  def callStackInfo =
    def getInfo(c: Context) = (
      c.func.id,
      c.name,
      c.cursor.stepsOpt.getOrElse(List()),
      c.locals.collect {
        case (Name(name), v) => (name, v.toString)
      }.toList,
    )
    (st.context :: st.callStack.map(_.context)).map(getInfo(_))

  /** context information */
  def ctxtInfo(cid: Int) =
    def paramInfo(p: IRFunc.Param) = (
      p.lhs.name,
      p.optional,
      p.ty.name,
    )

    val ctxts = (st.context :: st.callStack.map(_.context)).drop(cid)
    val (os, oe) = ctxts.flatMap(_.astOpt) match
      case curr :: _ if curr.loc.isDefined =>
        val loc = curr.loc.get
        (loc.start.offset, loc.end.offset)
      case _ => (-1, -1)
    val ctxt = ctxts.head
    val (nid, isExit) = ctxt.cursor match
      case NodeCursor(n) => (n.id, false)
      case _: ExitCursor => (-1, true)
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
