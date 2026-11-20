package esmeta.es.util.polyfill.dsl

import esmeta.lang.*
import esmeta.spec.Algorithm

import AstExtensions.*

class DSLPath(dslDir: String, stats: TransformStats = TransformStats())
  extends TransformPath {

  def apply(targets: List[Algorithm]) = {
    val rules = DSLRuleParser.parseDir(dslDir)
    targets.map { algo =>
      algo.copy(body = pass(algo.body.flatten, rules, stats))
    }
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
