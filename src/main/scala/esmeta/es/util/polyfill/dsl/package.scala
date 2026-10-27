package esmeta.es.util.polyfill.dsl

import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

import scala.collection.mutable

// -----------------------------------------------------------------------------
// annotated steps
// -----------------------------------------------------------------------------
/** Annotated Step: a Step paired with the analysis state at its entry point. */
case class AStep(
  step: Step,
  state: Analyzer.AbsState,
  children: List[AStep],
)

// -----------------------------------------------------------------------------
// unification context
// -----------------------------------------------------------------------------
object AstExtensions {
  extension (block: StepBlock) {
    def rawSteps: List[Step] = block.steps.map(_.step)
  }

  extension (steps: List[Step]) {
    def subSteps: List[SubStep] = steps.map(SubStep(Nil, _))
    def stepBlock: StepBlock = StepBlock(steps.subSteps)
    def blockStep: BlockStep = BlockStep(steps.stepBlock)
  }

  extension (step: Step) {
    def flatten: Step = {
      new LangWalker {
        override def walk(step: Step): Step =
          step match
            case BlockStep(StepBlock(List(SubStep(_, step)))) =>
              walk(step)
            case _ => super.walk(step)
      }.walk(step)
    }
  }
}

case class CaptureKey(name: String, variant: Int = 0)
type CaptureEnv = Map[CaptureKey, LangElem]

case class DSLContext(
  symbolicPaths: Map[String, List[String]] = Map.empty,
)
type LangElemPredicate = (LangElem, DSLContext) => Boolean

// -----------------------------------------------------------------------------
// rewriting statistics
// -----------------------------------------------------------------------------
class TransformStats {
  private val counts = mutable.Map[String, Int]().withDefaultValue(0)

  def record(ruleName: String): Unit = counts(ruleName) += 1

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
