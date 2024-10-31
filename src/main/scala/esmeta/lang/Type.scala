package esmeta.lang

import esmeta.lang.util.*
import esmeta.ty.*

/** metalanguage types */
case class Type(ty: Ty) extends Syntax {

  /** type name */
  lazy val name: String = ty match
    case UnknownTy(msg) => msg.getOrElse("unknown")
    case ty             => ty.toString

  /** normalized type name */
  lazy val normalizedName: String = Type.normalizeName(name)
}
object Type extends Parser.From(Parser.langTypeWithUnknown) {

  /** type name normalization */
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
    .stripSuffix("s")
}

/** unknown types */
def UnknownType: Type = Type(UnknownTy())
def UnknownType(msg: String): Type = Type(UnknownTy(Some(msg)))
