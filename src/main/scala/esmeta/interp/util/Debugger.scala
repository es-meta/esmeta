package esmeta.interp.util

import esmeta.cfg.{Block, Node}
import esmeta.interp.*
import esmeta.ir.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger breakpoints */
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

/** debugger extension of Interp */
abstract class Debugger(st: State) extends Interp(st, Nil) {

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed

  // step until predicate
  @tailrec
  final def stepUntil(pred: => Boolean): StepResult =
    if (!isBreaked) {
      val keep = step
      if (pred && keep) stepUntil(pred)
      else if (keep) StepResult.Succeed
      else StepResult.Terminated
    } else StepResult.Breaked

  // transition for node to more fine-grained execution within block node
  private var nextInstIdx: Option[Int] = None
  override def interp(node: Node): Unit = node match
    case block @ Block(_, insts, next) =>
      val instIdx = nextInstIdx.getOrElse(0)
      interp(insts(instIdx))
      if (instIdx + 1 == insts.length) { nextInstIdx = None; move(block)(next) }
    case _ =>
      super.interp(node)

  // trigger breakpoints for every call and instruction transition
  override def call(lhs: Id, fexpr: Expr, args: List[Expr]): Unit = {
    triggerBreaks; super.call(lhs, fexpr, args)
  }
  override def interp(inst: NormalInst): Unit = {
    triggerBreaks; super.interp(inst)
  }

  // continue
  final def continue: StepResult = stepUntil { true }

  // breakpoints
  val breakpoints = ListBuffer[Breakpoint]()

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
}
