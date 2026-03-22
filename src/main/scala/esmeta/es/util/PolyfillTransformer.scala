package esmeta.es.util

import esmeta.lang.*
import esmeta.spec.Algorithm

import scala.annotation.tailrec

import dsl.AstExtensions.*

class DSLPath(dslDir: String) extends OptimizationPath {

  def apply(targets: List[Algorithm]) = {
    val rules = dsl.DSLRuleParser.parseDir(dslDir)
    println(s"Using ${rules.length} internal rules")
    rules.foreach(r => println(s"  - ${r.name} (${r.getClass.getSimpleName})"))
    println()

    val stats = new dsl.TransformStats()

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
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    rules.foldLeft(body) { (curr, rule) =>
      val ctx = dsl.Analyzer.buildContext(curr)
      dsl.Transformer.transformStep(rule, curr, ctx, Some(stats))
    }
  }

  @tailrec
  private def fixpoint(
    body: Step,
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    val nextBody = pass(body, rules, stats)
    if (nextBody == body) body
    else fixpoint(nextBody, rules, stats)
  }

  // ===========================================================================
  // Predicates (from PredicateRegistry for reverse-lookup support)
  // ===========================================================================

  val isSetDataPredicate: dsl.LangElemPredicate =
    dsl.PredicateRegistry("isSetData")
  val isMapDataPredicate: dsl.LangElemPredicate =
    dsl.PredicateRegistry("isMapData")
  val isSameOrCopyOf: dsl.LangElemPredicate =
    dsl.PredicateRegistry("isSameOrCopyOf")
}
