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
) extends SpecElem

/** helper of ECMAScript specifications (ECMA-262) summary */
object Summary extends Parser.From[Summary](Parser.summary) {
  def apply(spec: Spec): Summary = if (!spec.isEmpty) {
    import ProductionKind.*
    val Spec(version, grammar, algos, tables, tyModel) = spec
    val Grammar(prods, prodsForWeb) = grammar
    val prodsBy = prods.groupBy(_.kind)
    val completeAlgos = spec.completeAlgorithms.length
    val completeSteps = spec.completeSteps.length
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
        complete = completeAlgos,
        incomplete = algos.length - completeAlgos,
      ),
      steps = StepSummary(
        complete = completeSteps,
        incomplete = spec.allSteps.length - completeSteps,
      ),
      types = TypeSummary(
        known = knownTypes,
        unknown = spec.types.length - knownTypes - yetTypes,
        yet = yetTypes,
      ),
      tables = tables.size,
      tyModel = tyModel.infos.size,
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
  complete: Int = 0,
  incomplete: Int = 0,
) {
  def total: Int = complete + incomplete
  def ratioString: String = percentString(complete, total)
}

/** algorithm step element */
case class StepSummary(
  complete: Int = 0,
  incomplete: Int = 0,
) {
  def total: Int = complete + incomplete
  def ratioString: String = percentString(complete, total)
}

/** type element */
case class TypeSummary(
  known: Int = 0,
  yet: Int = 0,
  unknown: Int = 0,
) {
  def total: Int = known + yet
  def ratioString: String = percentString(known, total)
}
