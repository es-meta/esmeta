package esmeta.lang

import esmeta.lang.util.*
import esmeta.ty.*

// metalanguage types
// TODO more detailed instead of strings
case class Type(ty: Ty = UnknownTy()) extends Syntax {
  // TODO refactor
  def name: String = ty.name
  def normalizedName: String = ty.normalizedName
}
object Type extends Parser.From[Type](Parser.langTy):
  // TODO refactor
  def apply(str: String): Type = Type(UnknownTy(str))
  def unapply(ty: Type): Option[String] = Some(ty.name)
