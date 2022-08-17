package esmeta.spec

import esmeta.lang.{Step, YetStep}
import esmeta.lang.util.*
import esmeta.spec.util.*
import org.jsoup.nodes.Element

/** abstract algorithms in ECMA-262 */
case class Algorithm(
  head: Head,
  elem: Element,
  body: Step,
  code: String,
) extends SpecElem {

  if(head.fname == "NumberBitwiseOp"){
    println(body)
  }

  /** check whether it is incomplete */
  lazy val complete: Boolean = incompleteSteps.isEmpty

  /** return types */
  lazy val retTy: Type = head.retTy

  /** get all steps */
  lazy val steps: List[Step] = StepCollector(body)

  /** get incomplete algorithm steps */
  lazy val incompleteSteps: List[Step] =
    steps.filter(_.isInstanceOf[YetStep])

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] =
    steps.filter(!_.isInstanceOf[YetStep])
}
