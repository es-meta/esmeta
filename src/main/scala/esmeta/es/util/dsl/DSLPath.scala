package esmeta.es.util.dsl

import esmeta.es.util.TransformPath
import esmeta.lang.*
import esmeta.spec.Algorithm

import scala.annotation.tailrec

import AstExtensions.*

class DSLPath(dslDir: String) extends TransformPath {

  def apply(targets: List[Algorithm]) = {
    val rules = DSLRuleParser.parseDir(dslDir)
    val stats = new TransformStats()

    val result = targets.map { algo =>
      algo.copy(body = pass(algo.body.flatten, rules, stats))
    }

    stats.printSummary()
    result
  }

  private def pass(
    body: Step,
    rules: List[Rule[LangElem]],
    stats: TransformStats,
  ): Step =
    rules.foldLeft(body) { (curr, rule) =>
      val astep = Analyzer.analyze(curr)
      Transformer.transformStep(rule, astep, Some(stats))
    }
}
