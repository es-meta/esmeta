package esmeta.lang

import esmeta.lang.util.*
import esmeta.ty.*

// metalanguage types
// TODO more detailed instead of strings
case class Type(ty: Ty = UnknownTy()) extends Syntax {
  // TODO refactor
  lazy val name: String = ty match
    case UnknownTy(msg) => msg.getOrElse("unknown")
    case _              => ???
  lazy val normalizedName: String = Type.normalizeName(name)
}
object Type extends Parser.From[Type](Parser.langType):
  // TODO refactor
  def normalizeName(name: String): String = {
    if (name startsWith "a ") name.drop(2)
    else if (name startsWith "an ") name.drop(3)
    else name
  }.replace("-", "")
    .replace("|", "")
    .trim
    .split(" ")
    .map(_.capitalize)
    .mkString
  def apply(str: String): Type = Type(UnknownTy(str))
  def unapply(ty: Type): Option[String] = ty.ty match
    case UnknownTy(Some(msg)) => Some(msg)
    case _                    => None
