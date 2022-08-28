package esmeta.lang

import esmeta.lang.util.*
import esmeta.typing.*

// metalanguage types
// TODO more detailed instead of strings
case class Type(ty: Ty) extends Syntax {
  // TODO refactor
  def name: String = ty.name
  def normalizedName: String = ty.normalizedName
}
object Type extends Parser.From[Type]:
  // TODO refactor
  def unapply(ty: Type): Option[String] = Some(ty.name)
val AnyType = Type(UnknownTy())
