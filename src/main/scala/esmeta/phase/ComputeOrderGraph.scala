package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.spec.Spec
import esmeta.parser.ESParser
import esmeta.transpile.Transpiler
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `compute-order-graph` phase */
case object ComputeOrderGraph extends Phase[CFG, (Ast, CFG)] {
  val name = "compute-order-graph"
  val help = "parses an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): (Ast, CFG) =
    given CFG = cfg
    val filename = getFirstFilename(cmdConfig, name)
    val ast = ESParser(cfg.grammar, config.debug)("Script").fromFile(filename)
    println("--------------ast---------------")
    println(ast)
    ast.getSdo("Evaluation") match
      case None => println("---------sdo not found-------")
      case Some(ast0 -> sdo) => {
        println("--------------ast0---------------")
        println(ast0)
        println("--------------sdo---------------")
        println(sdo)
        println("--------------sdo.toDot()---------------")
        {
          val pw = getPrintWriter("sdo.dot", false)
          pw.println(sdo.toDot())
          pw.flush()
        }
        SystemUtils.executeCmd(
          s"dot -Tpng sdo.dot -o sdo.png",
        )

        println("--------------og---------------")
        {
          import verify.*
          val pw = getPrintWriter("og.dot", false)
          pw.println(Transform(sdo, cfg).result.toDot)
          pw.flush()
        }
        SystemUtils.executeCmd(
          s"dot -Tpng og.dot -o og.png",
        )
      }
    (ast -> cfg)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(_.debug = _),
      "turn on debugging mode.",
    ),
  )
  case class Config(
    var debug: Boolean = false,
  )
}
