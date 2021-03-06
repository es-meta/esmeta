package esmeta.spec

import esmeta.{error => ESMetaError, *}
import esmeta.lang.*
import esmeta.spec.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.HtmlUtils.*
import org.jsoup.nodes.Document

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[Spec.Version] = None,
  grammar: Grammar = Grammar(),
  algorithms: List[Algorithm] = Nil,
  tables: Map[String, Table] = Map(),
  typeModel: TypeModel = TypeModel.js,
  document: Document = new Document(""),
) extends SpecElem {

  /** convert to an IR program */
  lazy val toIR: ir.Program = Compiler(this)

  /** convert to a control-flow graph (CFG) */
  lazy val toCFG: cfg.CFG = toIR.toCFG

  /** JavaScript parser */
  lazy val jsParser: js.util.Parser = js.util.Parser(grammar)

  /** get incomplete algorithms */
  lazy val incompleteAlgorithms: List[Algorithm] =
    algorithms.filter(!_.complete)

  /** get complete algorithms */
  lazy val completeAlgorithms: List[Algorithm] =
    algorithms.filter(_.complete)

  /** get all algorithm steps */
  lazy val allSteps: List[Step] = for {
    algo <- algorithms
    step <- algo.steps
  } yield step

  /** get incomplete algorithm steps */
  lazy val incompleteSteps: List[Step] =
    allSteps.filter(_.isInstanceOf[YetStep])

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] =
    allSteps.filter(!_.isInstanceOf[YetStep])

  /** mapping from algorithms names to algorithms */
  lazy val fnameMap: Map[String, Algorithm] =
    (for (algo <- algorithms) yield algo.head.fname -> algo).toMap

  /** get stats */
  lazy val stats: Stats = new Stats(this)

  /** get an algorithm by id attribute */
  def getAlgoById(id: String): Algorithm =
    algorithms.filter(_.elem.getId == id) match
      case algo :: Nil => algo
      case _           => error(s"no algorithms found for $id")
}
object Spec:
  case class Version(name: String, hash: String) extends SpecElem
