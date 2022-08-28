package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.ty.*

// TODO ir types
case class Type(ty: Ty = UnknownTy()) extends IRElem {
  // TODO refactor
  def name: String = ty.name
  def normalizedName: String = ty.normalizedName

  /** completion check */
  def isCompletion: Boolean = ty.isCompletion
}
object Type extends Parser.From[Type](Parser.ty):
  // TODO refactor
  def apply(str: String): Type = Type(UnknownTy(str))
  def unapply(ty: Type): Option[String] = Some(ty.name)
