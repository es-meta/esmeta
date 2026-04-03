package esmeta.es.util.dsl

import esmeta.es.util.TransformPath
import esmeta.lang.*
import esmeta.spec.Algorithm

import scala.annotation.tailrec

import AstExtensions.*

class DSLPath(dslDir: String) extends TransformPath {

  def apply(targets: List[Algorithm]) = {
    val rules = DSLRuleParser.parseDir(dslDir)
    println(s"Using ${rules.length} internal rules")
    rules.foreach { r =>
      println(s"${r.name} (${r.getClass.getSimpleName})")
      println(s"pattern: ${r.pattern}")
      println(s"replace: ${r.replace}")
      println("=" * 80)
    }
    println()

    val stats = new TransformStats()

    val result = targets.map { algo =>
      println(algo.name)
      val body = pass(algo.body.flatten, rules, stats)
      println("=" * 80)
      algo.copy(body = body)
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
