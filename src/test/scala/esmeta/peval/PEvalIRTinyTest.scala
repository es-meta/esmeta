package esmeta.peval

import esmeta.*
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map as MMap}

/** eval test */
class PEvalIRTinyTest extends PEvalTest {
  val name: String = "PEvalIRTinyTest"

  // registration
  def init: Unit =
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        PEvalIRTinyTest.interpFile(file.toString)
      }
    }

  init
}

object PEvalIRTinyTest {
  import esmeta.interpreter.Interpreter
  import esmeta.state.State
  import esmeta.cfgBuilder.CFGBuilder
  import esmeta.ir.Program
  import esmeta.peval.pstate.PState
  import esmeta.peval.pstate.PContext

  def interpFile(filename: String): State =
    val prog = Program.fromFile(filename)
    val pevaledProg = Program(
      prog.funcs.map {
        case f if !f.params.isEmpty => f
        case f => {
          PartialEvaluator
            .run(prog, f) { (_renamer, _pst) => /* do nothing */ }(
              log = false,
              detail = false,
              simplifyLevel = 0,
              logPW = None,
              timeLimit = None,
            )
        }
      },
      prog.spec,
    )
    Interpreter(State(CFGBuilder(pevaledProg)))
}
