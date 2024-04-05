package esmeta.injector

import esmeta.error.*
import esmeta.interpreter.Interpreter
import esmeta.ir.Return
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}

/** exit state extractor */
class ExitStateExtractor(val initSt: State, timeLimit: Option[Int] = None)
  extends Interpreter(initSt, timeLimit = timeLimit) {

  /** address name mapping */
  val addrNames: MMap[Addr, String] = MMap()

  /** transition for cursors and wrap errors with cursor info */
  override def eval(cursor: Cursor): Boolean =
    try super.eval(cursor)
    catch { case e: InterpreterError => throw InterpreterErrorAt(e, cursor) }

  /** hook return points to keep address name mapping */
  override def setReturn(value: Value, ret: Return): Unit = {
    super.setReturn(value, ret)
    if (this.st.context.name == "MakeBasicObject") {
      val contexts = (this.st.context :: this.st.callStack.map(_.context))
        .filter(c => c.func.isSDO || c.func.isBuiltin)
      (value, contexts) match
        case (addr: Addr, ctxt :: _) => addrNames += addr -> ctxt.name
        case _                       => /* do nothing */
    }
  }
}
