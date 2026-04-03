package esmeta.es.util.dsl

import scala.collection.mutable

class TransformStats {
  private val counts = mutable.Map[String, Int]().withDefaultValue(0)

  def record(ruleName: String): Unit = counts(ruleName) += 1

  def snapshot: Map[String, Int] = counts.toMap

  def printSummary(): Unit = {
    println()
    println("=== DSL Transformation Summary ===")
    counts.toList.sortBy(_._1).foreach {
      case (name, count) =>
        println(f"  $name%-50s : $count%3d")
    }
    val total = counts.values.sum
    println("-" * 60)
    println(f"  ${"Total"}%-50s : $total%3d")
    println()
  }
}
