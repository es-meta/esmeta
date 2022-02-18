package esmeta.ir

import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parser

/** IR programs */
case class Program(
  funcs: List[Func],
) extends IRElem {
  lazy val cfg: CFG = new Builder(this).result
}
object Program extends Parser.From[Program]
