package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.util.Parser

/** ECMAScript specifications (ECMA-262) summary */
case class Summary(
  version: Option[Spec.Version] = None, // git version
  grammar: Summary.GrammarElem = Summary.GrammarElem(), // grammar productions
  algos: Summary.AlgorithmElem = Summary.AlgorithmElem(), // abstract algorithms
  steps: Summary.StepElem = Summary.StepElem(), // abstract algorithms steps
  tables: Int = 0, // tables
  typeModel: Int = 0, // type models
) extends SpecElem

/** helper of ECMAScript specifications (ECMA-262) summary */
object Summary extends Parser.From[Summary] {
  def apply(spec: Spec): Summary = if (!spec.isEmpty) {
    import Production.Kind.*
    val Spec(version, grammar, algos, tables, typeModel, _) = spec
    val Grammar(prods, prodsForWeb) = grammar
    val prodsBy = prods.groupBy(_.kind)
    val completeAlgos = spec.completeAlgorithms.length
    val completeSteps = spec.completeSteps.length
    Summary(
      version = version,
      grammar = GrammarElem(
        lexical = prodsBy(Lexical).length,
        numeric = prodsBy(NumericString).length,
        syntactic = prodsBy(Syntactic).length,
        web = grammar.prodsForWeb.length,
      ),
      algos = AlgorithmElem(
        complete = completeAlgos,
        incomplete = algos.length - completeAlgos,
      ),
      steps = StepElem(
        complete = completeSteps,
        incomplete = spec.allSteps.length - completeSteps,
      ),
      tables = tables.size,
      typeModel = typeModel.infos.size,
    )
  } else Summary()

  /** grammar element */
  case class GrammarElem(
    lexical: Int = 0,
    numeric: Int = 0,
    syntactic: Int = 0,
    web: Int = 0,
  ) {
    def productions: Int = lexical + numeric + syntactic
    def total: Int = productions + web
  }

  /** algorithm element */
  case class AlgorithmElem(
    complete: Int = 0,
    incomplete: Int = 0,
  ) {
    def total: Int = complete + incomplete
    def ratioString: String = ratioSimpleString(complete, total)
  }

  /** algorithm step element */
  case class StepElem(
    complete: Int = 0,
    incomplete: Int = 0,
  ) {
    def total: Int = complete + incomplete
    def ratioString: String = ratioSimpleString(complete, total)
  }
}
