package esmeta.interpreter

import esmeta.ESMetaTest
import esmeta.cfgBuilder.*
import esmeta.ir.*
import esmeta.state.*

/** test for IR interpreter with a CFG */
trait InterpreterTest extends ESMetaTest {
  def category: String = "interpreter"
}
object InterpreterTest {
  // handle interpreter test
  def interp(st: State): State = Interpreter(st)
  def interpFile(filename: String): State =
    interp(State(CFGBuilder(Program.fromFile(filename))))
}
