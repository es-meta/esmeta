package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*

/** type declrmation */
case class TyDecl(
  name: String,
  parent: Option[String] = None,
  elems: List[TyDecl.Elem] = Nil,
) extends TyElem {
  import TyDecl.Elem.*

  /** type map */
  lazy val fieldMap: FieldMap = FieldMap(elems.map {
    case Method(name, optional, target) =>
      name -> OptValueTy(target.fold(CloT)(CloT(_)), optional)
    case Field(name, optional, typeStr) =>
      name -> OptValueTy(ValueTy.from(typeStr), optional)
  }.toMap)
}
object TyDecl extends Parser.From(Parser.tyDecl) {
  enum Elem extends TyElem {
    case Method(name: String, optional: Boolean, target: Option[String])
    case Field(name: String, optinoal: Boolean, typeStr: String)
    val name: String
  }
  object Elem extends Parser.From(Parser.tyDeclElem)
}
