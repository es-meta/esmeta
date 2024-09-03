package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.lang.{Type => LangType}
import esmeta.ty.*

/** IR types */
case class Type(
  ty: Ty,
  langTy: Option[LangType] = None,
) extends IRElem {

  /** definite type check */
  inline def isDefined: Boolean = ty.isDefined

  /** imprecise type check */
  inline def isImprec: Boolean = ty.isImprec

  /** completion check */
  inline def isCompletion: Boolean = ty.isCompletion
}
object Type extends Parser.From(Parser.irType)

/** IR unknown types */
val UnknownType: Type = Type(UnknownTy())
def UnknownType(
  msg: String,
  langTy: Option[LangType] = None,
): Type = Type(UnknownTy(Some(msg)), langTy)
