package esmeta.ir

import esmeta.DEBUG
import esmeta.error._
import esmeta.util.Useful._
import esmeta.util._
import scala.annotation.{ tailrec, targetName }
import scala.collection.mutable.{ Map => MMap }

// IR Interpreter
class Interp(
  val st: State,
  timeLimit: Option[Long]
) {
  import Interp._

  val cursorGen: CursorGen[_ <: Cursor] = st.cursorGen

  // set start time of interpreter
  val startTime: Long = System.currentTimeMillis

  // the number of instructions
  def getIter: Int = iter
  private var iter: Int = 0

  // maximum callstack size
  private var maxDepth: Int = 1
  private def updateCallDepth() = {
    val d = st.ctxtStack.size + 1
    if (d > maxDepth) maxDepth = d
  }

  // iteration period for check
  val CHECK_PERIOD = 10000

  // step target
  trait StepTarget {
    override def toString: String = this match {
      case Terminate => "TERMINATED"
      case ReturnUndef => "RETURN"
      case NextStep(cursor) => ??? // cursor.toString()
    }
  }
  case object Terminate extends StepTarget
  case object ReturnUndef extends StepTarget
  case class NextStep(cursor: Cursor) extends StepTarget

  // get next step target
  def nextTarget: StepTarget = st.context.cursorOpt match {
    case Some(cursor) => NextStep(cursor)
    case None => st.ctxtStack match {
      case Nil => Terminate
      case _ => ReturnUndef
    }
  }

  // step
  final def step: Boolean = nextTarget match {
    case Terminate =>
      false
    case ReturnUndef =>
      // do return
      doReturn(Value.Undef)

      // keep going
      true
    case NextStep(cursor) => {
      iter += 1

      // check time limit
      if (iter % CHECK_PERIOD == 0) timeLimit.map(limit => {
        val duration = (System.currentTimeMillis - startTime) / 1000
        if (duration > limit) ???
      })

      // text-based debugging
      if (DEBUG) cursor match {
        case InstCursor(Inst.ISeq(_), _) =>
        case _ => ???
          // println(s"[$iter] ${st.context.name}: ${cursor.toString()}")
      }

      // interp the current cursor
      catchReturn(cursor match {
        case cursor @ InstCursor(inst, rest) =>
          interp(inst, rest)
      })

      // garbage collection
      if (iter % 100000 == 0) ??? // GC(st)

      // keep going
      true
    }
  }

  // fixpoint
  @tailrec
  final def fixpoint: State = step match {
    case true => fixpoint
    case false => st
  }

  // transition for instructions
  def interp(inst: Inst, rest: List[Inst]): Unit = inst match {
    case inst: Inst.ISeq => interp(inst, rest)
    case inst: CondInst => interp(inst, rest)
    case inst: CallInst => interp(inst)
    case inst: ArrowInst => interp(inst)
    case inst: NormalInst => interp(inst)
  }

  // transition for sequence instructions
  def interp(inst: Inst.ISeq, rest: List[Inst]): Unit = ???

  // transition for conditional instructions
  @targetName("interpCondInst")
  def interp(inst: CondInst, rest: List[Inst]): Unit = ???
  
  // transition for call instructions
  @targetName("interpCallInst")
  def interp(inst: CallInst): Unit = ???
  
  // transition for normal instructions
  @targetName("interpNormalInst")
  def interp(inst: NormalInst): Unit = ???

  // transition for arrow instructions
  @targetName("interpArrowInst")
  def interp(inst: ArrowInst): Unit = ???

  // catch return values
  def catchReturn(f: => Unit): Unit = ???

  // return value
  private case class ReturnValue(value: Value) extends Throwable

  // return helper
  def doReturn(value: Value): Unit = ???

  // expresssions
  def interp(expr: Expr): Value = ???

  // return if abrupt completion
  def returnIfAbrupt(value: Value, check: Boolean): Value = ???

  // references
  def interp(ref: Ref): RefValue = ???

  // short circuit evaluation
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value = ???
}

// interp object
object Interp {
  def apply(
    st: State,
    timeLimit: Option[Long]
  ): State = {
    val interp = new Interp(st, timeLimit)
    interp.fixpoint
    st
  }

  // unary operators
  def interp(uop: UOp, operand: Value): Value = ???

  // binary operators
  def interp(bop: BOp, left: Value, right: Value): Value = ???
}
