package esmeta.injector

import esmeta.error.*
import esmeta.es.builtin.JOB_QUEUE
import esmeta.interpreter.Interpreter
import esmeta.ir.{ERef, Global, IPop, NormalInst, Return}
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}

/** exit state extractor */
class ExitStateExtractor(
  val initSt: State,
  timeLimit: Option[Int],
) extends Interpreter(initSt, timeLimit = timeLimit) {

  /** address name mapping */
  val addrNames: MMap[Addr, String] = MMap()

  /** state once the script has run, before any queued job has */
  var scriptSt: Option[State] = None

  /** the script is the first job, so the next dequeue is where it ended */
  private var dequeued: Int = 0

  /** transition for cursors and wrap errors with cursor info */
  override def eval(cursor: Cursor): Boolean =
    try super.eval(cursor)
    catch { case e: InterpreterError => throw InterpreterErrorAt(e, cursor) }

  /** hook the job queue to keep the state the injected assertions will see */
  override def eval(inst: NormalInst): Unit =
    inst match
      case IPop(_, ERef(Global(JOB_QUEUE)), _) =>
        dequeued += 1
        if (dequeued == 2) scriptSt = Some(st.copied)
      case _ => /* do nothing */
    super.eval(inst)

  /** hook return points to keep address name mapping */
  // override def setReturn(value: Value, ret: Return): Unit =
  //   super.setReturn(value, ret)
  //   if (this.st.context.name == "MakeBasicObject") {
  //     val contexts = (this.st.context :: this.st.callStack.map(_.context))
  //       .filter(c => c.func.isSDO || c.func.isBuiltin)
  //     (value, contexts) match
  //       case (addr: Addr, ctxt :: _) => addrNames += addr -> ctxt.name
  //       case _                       => /* do nothing */
  //   }
}
