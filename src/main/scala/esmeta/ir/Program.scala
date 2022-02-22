package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.spec.Spec
import esmeta.util.BaseUtils.*

/** IR programs */
case class Program(
  funcs: List[Func] = Nil,
) extends IRElem {
  // backward edge to a specification
  var spec: Spec = Spec()

  // the main function
  lazy val main: Func = getUnique(funcs, _.main, "main function")
}

object Program extends Parser.From[Program]
