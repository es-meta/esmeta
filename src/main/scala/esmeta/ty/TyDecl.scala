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

  lazy val typeMap: Map[String, (Boolean, ValueTy)] = elems.map {
    case Method(name, optional, target) =>
      name -> (optional, target.fold(CloT)(CloT(_)))
    case Field(name, optional, typeStr) =>
      name -> (optional, ValueTy.from(typeStr))
  }.toMap

  // TODO move to TyModel
  lazy val methods: Map[String, String] = (for {
    (field, (opt, ty)) <- typeMap
    fname <- ty.clo match
      case Fin(set) if !opt && set.size == 1 => Some(set.head)
      case _                                 => None
  } yield field -> fname).toMap
}
object TyDecl extends Parser.From(Parser.tyDecl) {
  enum Elem extends TyElem {
    case Method(name: String, optional: Boolean, target: Option[String])
    case Field(name: String, optinoal: Boolean, typeStr: String)
    val name: String
  }
  object Elem extends Parser.From(Parser.tyDeclElem)
}
