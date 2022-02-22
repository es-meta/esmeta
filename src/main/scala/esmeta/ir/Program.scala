package esmeta.ir

import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parser
import esmeta.spec.Spec

/** IR programs */
case class Program(
  funcs: List[Func], // compiled functions from abstract algorithms
  spec: Option[Spec] = None, // an optional backward edge to specification
) extends IRElem {
  lazy val cfg: CFG = new Builder(this).result // XXX refactor and remove
}
object Program extends Parser.From[Program] // TODO update
