package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.typing.*

// TODO ir types
case class Type(ty: Ty) extends IRElem {
  // TODO refactor
  def name: String = ty.name
  def normalizedName: String = ty.normalizedName

  /** completion check */
  def isCompletion: Boolean = ty.isCompletion
}
object Type extends Parser.From[Type]:
  // TODO refactor
  def unapply(ty: Type): Option[String] = Some(ty.name)
val UnknownType = Type(UnknownTy())
