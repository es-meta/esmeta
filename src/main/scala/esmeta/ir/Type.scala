package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.ty.*

// TODO ir types
case class Type(ty: Ty = UnknownTy()) extends IRElem {

  /** completion check */
  def isCompletion: Boolean = ty.isCompletion
}
object Type extends Parser.From[Type](Parser.irType):
  // TODO refactor
  def apply(str: String): Type = Type(UnknownTy(str))
