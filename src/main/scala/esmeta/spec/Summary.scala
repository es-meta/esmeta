package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.util.Parser

/** ECMAScript specifications (ECMA-262) summary */
case class Summary(
  version: Option[Spec.Version] = None, // git version
  grammar: GrammarSummary = GrammarSummary(), // grammar productions
  algos: AlgorithmSummary = AlgorithmSummary(), // abstract algorithms
  steps: StepSummary = StepSummary(), // abstract algorithms steps
  types: TypeSummary = TypeSummary(), // types
  tables: Int = 0, // tables
  tyModel: Int = 0, // type models
  intrinsics: Int = 0, // intrinsics
) extends SpecElem

/** helper of ECMAScript specifications (ECMA-262) summary */
object Summary extends Parser.From[Summary](Parser.summary) {
  def apply(spec: Spec): Summary = if (!spec.isEmpty) {
    import ProductionKind.*
    val Spec(version, grammar, algos, tables, tyModel, intrinsics) = spec
    val Grammar(prods, prodsForWeb) = grammar
    val prodsBy = prods.groupBy(_.kind)
    val completeAlgos = spec.completeAlgorithms.length
    val completeSteps = spec.completeSteps.length
    val equalAlgos = spec.algorithms.filter(_.equals).length
    val knownTypes = spec.knownTypes.length
    val yetTypes = spec.yetTypes.length
    Summary(
      version = version,
      grammar = GrammarSummary(
        lexical = prodsBy(Lexical).length,
        numeric = prodsBy(NumericString).length,
        syntactic = prodsBy(Syntactic).length,
        web = grammar.prodsForWeb.length,
      ),
      algos = AlgorithmSummary(
        total = algos.length,
        complete = completeAlgos,
        equal = equalAlgos,
      ),
      steps = StepSummary(
        total = spec.allSteps.length,
        complete = completeSteps,
      ),
      types = TypeSummary(
        total = spec.types.length,
        known = knownTypes,
        yet = yetTypes,
      ),
      tables = tables.size,
      tyModel = tyModel.decls.size,
      intrinsics = intrinsics.models.length,
    )
  } else Summary()
}

/** grammar element */
case class GrammarSummary(
  lexical: Int = 0,
  numeric: Int = 0,
  syntactic: Int = 0,
  web: Int = 0,
) {
  def productions: Int = lexical + numeric + syntactic
  def total: Int = productions + web
}

/** algorithm element */
case class AlgorithmSummary(
  total: Int = 0,
  complete: Int = 0,
  equal: Int = 0,
) {
  def incomplete: Int = total - complete
  def inequal: Int = total - equal
  def completeRatio: String = percentString(complete, total)
  def equalRatio: String = percentString(equal, total)
}

/** algorithm step element */
case class StepSummary(
  total: Int = 0,
  complete: Int = 0,
) {
  def incomplete: Int = total - complete
  def completeRatio: String = percentString(complete, total)
}

/** type element */
case class TypeSummary(
  total: Int = 0,
  known: Int = 0,
  yet: Int = 0,
) {
  def unknown: Int = total - known - yet
  def completeRatio: String = percentString(known, total)
  def yetRatio: String = percentString(yet, total)
}
