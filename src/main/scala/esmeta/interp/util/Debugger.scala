package esmeta.interp.util

import esmeta.DEBUG
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.lang.Syntax
import esmeta.spec.Algorithm
import esmeta.util.Loc
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger extension of ir interpreter */
class Debugger(
  st: State,
  val breakpoints: ListBuffer[Breakpoint] = ListBuffer(),
) extends Interp(st, Nil) {
  // DEBUG = true

  // ------------------------------------------------------------------------------
  // shortcuts
  // ------------------------------------------------------------------------------
  private inline def cfg = st.cfg
  private inline def cursor = st.context.cursor
  private inline def func = cursor.func
  private inline def irFunc = func.irFunc
  private inline def algoOpt: Option[Algorithm] = irFunc.algo
  private inline def instOpt: Option[Inst] = cursor.instOpt(instIdx)
  private inline def langOpt: Option[Syntax] = cursor.langOpt(instIdx)
  private inline def locOpt: Option[Loc] = cursor.locOpt(instIdx)

  // ------------------------------------------------------------------------------
  // overrides interpreter
  // ------------------------------------------------------------------------------
  // transition for node to more fine-grained execution within block node
  private var instIdx: Option[Int] = None
  override def step: Boolean = (super.step, cursor) match
    case (_, _: ExitCursor) => super.step
    case (res, _)           => res
  override def interp(node: Node): Unit = node match
    case block @ Block(_, insts, next) =>
      val idx = instIdx.getOrElse(0)
      interp(insts(idx))
      if (idx + 1 == insts.length) { instIdx = None; st.context.moveNext }
      else instIdx = Some(idx + 1)
    case _ =>
      super.interp(node)
  override def setReturn(value: Value): Unit = {
    instIdx = None; super.setReturn(value)
  }

  // trigger breakpoints for every call and instruction transition
  override def call(lhs: Id, fexpr: Expr, args: List[Expr]): Unit = {
    super.call(lhs, fexpr, args); triggerBreaks
  }
  override def interp(inst: NormalInst): Unit = {
    triggerBreaks; super.interp(inst)
  }

  // ------------------------------------------------------------------------------
  // execution control
  // ------------------------------------------------------------------------------

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed

  // step until given predicate
  @tailrec
  final def stepUntil(pred: => Boolean): StepResult =
    if (!isBreaked) {
      val keep = step
      if (pred && keep) stepUntil(pred)
      else if (keep) StepResult.Succeed
      else StepResult.Terminated
    } else StepResult.Breaked

  // spec step
  final def specStep =
    val (prevName, prevLoc) = (irFunc.name, locOpt)
    stepUntil { prevName == irFunc.name && prevLoc == locOpt }

  // spec step over
  final def specStepOver =
    val prev = st.callStack.size
    stepUntil { prev != st.callStack.size }

  // spec step out
  final def specStepOut =
    val prev = st.callStack.size
    stepUntil { prev <= st.callStack.size }

  // TODO js steps from ast span info
  // final def jsStep = ???
  // final def jsStepOver = ???
  // final def jsStepOut = ???

  // continue
  final def continue: StepResult = stepUntil { true }

  // ------------------------------------------------------------------------------
  // breakpoints
  // ------------------------------------------------------------------------------

  // trigger breakpoints
  private def triggerBreaks: Unit = for (b <- breakpoints) b.check(st)

  // remove breakpoints
  final def rmBreak(opt: String) = opt match
    case "all" => breakpoints.clear
    case idx   => breakpoints.remove(idx.toInt)

  // toggle breakpoints
  final def toggleBreak(opt: String) = opt match
    case "all" => for { bp <- breakpoints } bp.toggle()
    case idx   => breakpoints(idx.toInt).toggle()

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
    def instOpt(idx: Option[Int] = None): Option[Inst] = cursor match
      case NodeCursor(node) =>
        node match
          case Block(_, insts, _) => Some(insts(idx.getOrElse(0)))
          case node: NodeWithInst => node.inst
      case _: ExitCursor => None

    /** get syntax object of current cursor */
    def langOpt(idx: Option[Int] = None) =
      cursor.instOpt(idx).fold(None)(_.langOpt)

    /** get location in spec of current cursor */
    def locOpt(idx: Option[Int] = None) = cursor.langOpt(idx).fold(None)(_.loc)
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
      c.cursor.locOpt(instIdx).map(_.steps).getOrElse(List()),
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
