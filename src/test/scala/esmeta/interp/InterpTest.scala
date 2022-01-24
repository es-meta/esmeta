package esmeta.interp

import esmeta.cfg.*
import esmeta.ESMetaTest

trait InterpTest extends ESMetaTest {
  def category: String = "interp"

  // handle interp test
  def interp(st: State): State =
    new Interp(st).fixpoint
  def interp(str: String): State =
    interp(State(CFG.from(str)))
  def interpFile(filename: String): State =
    interp(State(CFG.fromFile(filename)))
}
