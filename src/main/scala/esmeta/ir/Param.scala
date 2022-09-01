package esmeta.ir

import esmeta.ir.util.Parser

/** IR function parameters */
case class Param(
  lhs: Name,
  ty: Type = UnknownType,
  optional: Boolean = false,
) extends IRElem
object Param extends Parser.From(Parser.param)
