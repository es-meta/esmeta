package esmeta.ty

import esmeta.ty.util.Parser

/** types */
trait Ty extends TyElem {

  /** completion check */
  def isDefined: Boolean = this match
    case _: UnknownTy => false
    case _            => true

  /** completion check */
  def isCompletion: Boolean
}
object Ty extends Parser.From(Parser.ty)
