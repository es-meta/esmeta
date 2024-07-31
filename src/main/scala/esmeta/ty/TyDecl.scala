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
      name -> handleOption(optional, target.fold(CloT)(CloT(_)))
    case Field(name, optional, typeStr) =>
      name -> handleOption(optional, ValueTy.from(typeStr))
  }.toMap)

  private def handleOption(bool: Boolean, ty: ValueTy): ValueTy =
    if (bool) ty || AbsentT else ty
}
object TyDecl extends Parser.From(Parser.tyDecl) {
  enum Elem extends TyElem {
    case Method(name: String, optional: Boolean, target: Option[String])
    case Field(name: String, optinoal: Boolean, typeStr: String)
    val name: String
  }
  object Elem extends Parser.From(Parser.tyDeclElem)
}
