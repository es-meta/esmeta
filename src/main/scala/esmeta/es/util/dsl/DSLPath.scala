package esmeta.es.util.dsl

import esmeta.es.util.OptimizationPath
import esmeta.lang.*
import esmeta.spec.Algorithm

import scala.annotation.tailrec

import AstExtensions.*

class DSLPath(dslDir: String) extends OptimizationPath {

  def apply(targets: List[Algorithm]) = {
    val rules = DSLRuleParser.parseDir(dslDir)
    println(s"Using ${rules.length} internal rules")
    rules.foreach(r => println(s"  - ${r.name} (${r.getClass.getSimpleName})"))
    println()

    val stats = new TransformStats()

    val result = targets.map { algo =>
      // println(s"[*] Processing ${algo.head.fname}")
      val body = pass(algo.body, rules, stats)
      // println("=" * 80)
      // println()
      algo.copy(body = body)
    }

    stats.printSummary()
    result
  }

  def pass(
    body: Step,
    rules: List[Rule],
    stats: TransformStats,
  ): Step = {
    rules.foldLeft(body) { (curr, rule) =>
      val ctx = Analyzer.buildContext(curr)
      Transformer.transformStep(rule, curr, ctx, Some(stats))
    }
  }

  @tailrec
  private def fixpoint(
    body: Step,
    rules: List[Rule],
    stats: TransformStats,
  ): Step = {
    val nextBody = pass(body, rules, stats)
    if (nextBody == body) body
    else fixpoint(nextBody, rules, stats)
  }

  // ===========================================================================
  // Predicates (from PredicateRegistry for reverse-lookup support)
  // ===========================================================================

  val isSetDataPredicate: LangElemPredicate =
    PredicateRegistry("isSetData")
  val isMapDataPredicate: LangElemPredicate =
    PredicateRegistry("isMapData")
  val isSameOrCopyOf: LangElemPredicate =
    PredicateRegistry("isSameOrCopyOf")
}
