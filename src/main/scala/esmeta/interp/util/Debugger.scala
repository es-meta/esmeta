package esmeta.interp.util

import esmeta.LOG
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.lang.Syntax
import esmeta.spec.Algorithm
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
      case block @ Block(_, insts, next) =>
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

  private def getInfo = ((irFunc.name, cursor.stepsOpt), st.callStack.size)

  // spec step
  final def specStep = {
    val (prevLoc, _) = getInfo
    stepUntil { prevLoc == getInfo._1 }
  }

  // spec step over
  final def specStepOver =
    val (prevLoc, prevStackSize) = getInfo
    stepUntil {
      val (loc, stackSize) = getInfo
      (prevLoc == loc) || (prevStackSize < stackSize)
    }

  // spec step out
  final def specStepOut =
    val (_, prevStackSize) = getInfo
    stepUntil { prevStackSize <= getInfo._2 }

  // TODO js steps from ast span info
  // private def getJsInfo = ???
  // final def jsStep = ???
  // final def jsStepOver = ???
  // final def jsStepOut = ???

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

  /** heap information */
  def heapInfo = st.heap.map.map {
    case (addr, obj) => (addr.toString, obj.toString)
  }

  /** call stack information */
  def callStackInfo =
    def ctxtInfo(c: Context) = (
      c.func.id,
      c.name,
      c.cursor.stepsOpt.getOrElse(List()),
      c.locals.collect {
        case (Name(name), v) => (name, v.toString)
      }.toList,
    )
    (st.context :: st.callStack.map(_.context)).map(ctxtInfo(_))

  /** func information */
  def funcInfo(fid: Int) =
    def paramInfo(p: IRFunc.Param) = (
      p.lhs.name,
      p.optional,
      p.ty.name,
    )
    val irFunc = cfg.funcMap(fid).irFunc
    val code = irFunc.algo.map(_.code).getOrElse("")
    (
      irFunc.kind.ordinal,
      irFunc.name,
      irFunc.params.map(paramInfo(_)),
      irFunc.body.toString,
      code,
    )
}
