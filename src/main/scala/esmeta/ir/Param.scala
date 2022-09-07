package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.spec.{Param => SpecParam}

/** IR function parameters */
case class Param(
  lhs: Name,
  ty: Type = UnknownType,
  optional: Boolean = false,
  specParam: Option[SpecParam] = None,
) extends IRElem
object Param extends Parser.From(Parser.param)
