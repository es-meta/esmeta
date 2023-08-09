package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.spec.{Param => SpecParam}
import esmeta.ty.*

/** IR function parameters */
case class Param(
  lhs: Name,
  ty: Type = UnknownType,
  optional: Boolean = false,
  specParam: Option[SpecParam] = None,
) extends IRElem {
  def getTy: ValueTy = (ty.ty match
    case _: UnknownTy => BotT
    case ty: ValueTy  => ty
  ) || (if (optional) AbsentT else BotT)
}
object Param extends Parser.From(Parser.param)
