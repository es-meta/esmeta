package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

object AstExtensions {
  extension (block: StepBlock) {
    def rawSteps: List[Step] = block.steps.map(_.step)
  }

  extension (steps: List[Step]) {
    def subSteps: List[SubStep] = steps.map(SubStep(None, _))
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
