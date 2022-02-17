package esmeta.interp

import esmeta.ir.Program
import esmeta.ir.util.Parser
import esmeta.ir.{Func => IRFunc, *}
import esmeta.cfg.util.*
import esmeta.ESMetaTest

trait InterpTest extends ESMetaTest {
  def category: String = "interp"

  // handle interp test
  def interp(st: State): State =
    new Interp(st).fixpoint
  def interpFile(filename: String): State =
    interp(State(Program.fromFile(filename).toCFG))
}
