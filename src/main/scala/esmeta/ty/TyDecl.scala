package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*

/** type declrmation */
case class TyDecl(
  name: String,
  parent: Option[String] = None,
  elems: List[TyDecl.Elem] = Nil,
) extends TyElem

object TyDecl extends Parser.From(Parser.tyDecl) {
  enum Elem extends TyElem {
    case AbsMethod(name: String)
    case ConMethod(name: String, optional: Boolean, target: Option[String])
    case Field(name: String, optional: Boolean, typeStr: String)
    val name: String
  }
  object Elem extends Parser.From(Parser.tyDeclElem)
}
