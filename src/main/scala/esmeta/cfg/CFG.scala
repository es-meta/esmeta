package esmeta.cfg

import esmeta.*
import esmeta.cfg.util.*
import esmeta.ir.Program
import esmeta.parser.{ESParser, AstFrom}
import esmeta.spec.{Spec, Grammar}
import esmeta.ty.TyModel
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.ProgressBar
import esmeta.util.SystemUtils.*

/** control-flow graphs (CFGs) */
case class CFG(
  funcs: List[Func] = Nil,
) extends CFGElem {

  /** backward edge to a program */
  var program: ir.Program = ir.Program()

  /** the main function */
  lazy val main: Func = getUnique(funcs, _.irFunc.main, "main function")

  /** an ECMAScript parser */
  lazy val esParser: ESParser = program.esParser
  lazy val scriptParser: AstFrom = esParser("Script")

  /** mapping from fid to functions */
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap

  /** mapping from function names to functions */
  lazy val fnameMap: Map[String, Func] =
    (for (func <- funcs) yield func.irFunc.name -> func).toMap

  /** all nodes */
  lazy val nodes: List[Node] = funcs.flatMap(_.nodes)

  /** mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] = (for {
    func <- funcs
    node <- func.nodes
  } yield node.id -> node).toMap

  /** mapping from nodes to functions */
  lazy val funcOf: Map[Node, Func] = (for {
    func <- funcs
    node <- func.nodes
  } yield node -> func).toMap

  /** get a type model */
  def tyModel: TyModel = spec.tyModel

  /** get the corresponding specification */
  def spec: Spec = program.spec

  /** get the corresponding grammar */
  def grammar: Grammar = spec.grammar

  /** dump CFG */
  def dumpTo(baseDir: String): Unit =
    val dirname = s"$baseDir/func"
    dumpDir(
      name = "CFG functions",
      iterable = ProgressBar("Dump CFG functions", funcs, detail = false),
      dirname = dirname,
      getName = func => s"${func.normalizedName}.cfg",
    )

  /** dump in a DOT format */
  def dumpDot(
    baseDir: String,
    pdf: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    val format = if (pdf) "DOT/PDF formats" else "a DOT format"
    val progress = ProgressBar(
      msg = s"Dump CFG functions in $format",
      iterable = funcs,
      getName = (x, _) => x.name,
      detail = false,
      concurrent = CP.Auto,
    )
    for (func <- progress)
      val path = s"$baseDir/${func.normalizedName}"
      val dotPath = s"$path.dot"
      val pdfPath = if (pdf) Some(s"$path.pdf") else None
      func.dumpDot(dotPath, pdfPath)
}
