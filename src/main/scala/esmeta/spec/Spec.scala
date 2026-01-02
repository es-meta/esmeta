package esmeta.spec

import esmeta.*
import esmeta.compiler.Compiler
import esmeta.lang.*
import esmeta.es.builtin.Intrinsics
import esmeta.parser.{ESParser, AstFrom}
import esmeta.spec.util.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.Git
import esmeta.util.HtmlUtils.*
import org.jsoup.nodes.Document

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[Spec.Version] = None, // git version
  grammar: Grammar = Grammar(), // lexical/syntactic grammar productions
  algorithms: List[Algorithm] = Nil, // abstract algorithms for semantics
  tables: Map[String, Table] = Map(), // tables
  tyModel: TyModel = TyModel(), // type models
  intrinsics: Intrinsics = Intrinsics(), // intrinsics
) extends SpecElem {

  /** HTML Document element */
  var document: Document = Document("")

  /** ECMAScript parser */
  lazy val esParser: ESParser = ESParser(grammar)
  lazy val scriptParser: AstFrom = esParser("Script")

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

  /** get incomplete algorithm steps
    *
    * @see
    *   [[yetSteps]], [[yetConds]]
    */
  lazy val incompleteSteps: List[Step] = allSteps.filter(!_.complete)

  private lazy val yetStepsAndConds: (List[Step], List[Condition]) = {
    var yetSteps = Vector[Step]()
    var yetConds = Vector[Condition]()
    for (step <- incompleteSteps) step match
      case IfStep(cond, _, _, _) => yetConds :+= cond
      case AssertStep(cond)      => yetConds :+= cond
      case _                     => yetSteps :+= step
    (yetSteps.toList, yetConds.toList)
  }

  /** get incomplete algorithm steps, refined from [[incompleteSteps]]
    *
    * @see
    *   [[incompleteSteps]]
    */
  lazy val yetSteps: List[Step] = yetStepsAndConds._1

  /** get incomplete algorithm conditions, refined from [[incompleteSteps]]
    *
    * @see
    *   [[incompleteSteps]]
    */
  lazy val yetConds: List[Condition] = yetStepsAndConds._2

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] = allSteps.filter(_.complete)

  /** get all types */
  lazy val types: List[Type] = for {
    algo <- algorithms
    ty <- algo.types
  } yield ty

  /** get known types */
  lazy val knownTypes: List[Type] =
    types.collect { case ty @ Type(_: ValueTy) => ty }

  /** get unknown types with `msg` defined. */
  lazy val yetTypes: List[Type] =
    types.collect { case ty @ Type(UnknownTy(Some(_))) => ty }

  /** mapping from algorithms names to algorithms */
  lazy val fnameMap: Map[String, Algorithm] =
    (for (algo <- algorithms) yield algo.head.fname -> algo).toMap

  /** get stats */
  lazy val stats: Stats = new Stats(this)

  /** get summary */
  lazy val summary: Summary = Summary(this)

  /** get an algorithm by id attribute */
  def getAlgoById(id: String): Algorithm =
    algorithms.find(_.elem.getId == id) match
      case Some(algo) => algo
      case None       => raise(s"no algorithms found for $id")

  /** empty check */
  def isEmpty: Boolean =
    version == None &&
    grammar == Grammar() &&
    algorithms.isEmpty &&
    tables.isEmpty &&
    tyModel == TyModel() &&
    intrinsics == Intrinsics()

  /** ECMAScript version string */
  lazy val versionString: String = version.fold("<none>")(_.toString)
}
object Spec extends Git(ECMA262_DIR)
