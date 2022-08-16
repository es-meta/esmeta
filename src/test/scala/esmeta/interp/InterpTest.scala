package esmeta.interp

import esmeta.ir.util.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.cfgbuilder.*
import esmeta.ESMetaTest

trait InterpTest extends ESMetaTest {
  def category: String = "interp"
}
object InterpTest {
  // handle interp test
  def interp(st: State): State = Interp(st)
  def interpFile(filename: String): State =
    interp(State(CFGBuilder(Program.fromFile(filename))))
}
