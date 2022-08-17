package esmeta.ir

import esmeta.*
import esmeta.ir.util.{Parser, YetCollector}
import esmeta.parser.{ESParser, AstFrom}
import esmeta.spec.{Spec, TypeModel}
import esmeta.util.BaseUtils.*
import esmeta.util.ProgressBar
import esmeta.util.SystemUtils.*

/** IR programs */
case class Program(
  funcs: List[Func] = Nil, // IR functions
) extends IRElem {

  /** backward edge to a specification */
  var spec: Spec = Spec()

  /** the main function */
  lazy val main: Func = getUnique(funcs, _.main, "main function")

  /** convert to a control-flow graph (CFG) */
  lazy val toCFG: cfg.CFG = cfgbuilder.CFGBuilder(this)

  /** ECMAScript parser */
  lazy val esParser: ESParser = spec.esParser
  lazy val scriptParser: AstFrom = esParser("Script")

  /** get list of all yet expressions */
  lazy val yets: List[EYet] = YetCollector(this)

  /** get a type model */
  def typeModel: TypeModel = spec.typeModel

  /** dump IR program */
  def dumpTo(baseDir: String): Unit =
    val dirname = s"$baseDir/func"
    dumpDir(
      name = "IR functions",
      ts = ProgressBar("Dump IR functions", funcs),
      dirname = dirname,
      getPath = func => s"$dirname/${func.normalizedName}.ir",
    )
}
object Program extends Parser.From[Program] {
  def apply(funcs: List[Func], spec: Spec): Program =
    val program = Program(funcs)
    program.spec = spec
    program
}
