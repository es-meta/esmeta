package esmeta.ir

import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parser
import esmeta.spec.Spec
import esmeta.spec.TypeModel
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

/** IR programs */
case class Program(
  funcs: List[Func] = Nil,
) extends IRElem {
  // backward edge to a specification
  var spec: Spec = Spec()

  // the main function
  lazy val main: Func = getUnique(funcs, _.main, "main function")

  /** convert to a control-flow graph (CFG) */
  lazy val toCFG: CFG = Builder(this)

  /** get a type model */
  def typeModel: TypeModel = spec.typeModel

  /** dump program of specs */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    for {
      func <- funcs
      name = func.name
      filename = s"$baseDir/${name.replace("/", "")}.ir"
    } dumpFile(func, filename)
}

object Program extends Parser.From[Program]
