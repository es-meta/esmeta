package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*

/** type declrmation */
case class TyDecl(
  name: String,
  parent: Option[String] = None,
  rawFields: Map[String, String] = Map(),
) extends TyElem {
  lazy val fields: Map[String, ValueTy] = for {
    (field, typeStr) <- rawFields
  } yield field -> ValueTy.from(typeStr)

  // TODO move to TyModel
  lazy val methods: Map[String, String] = (for {
    (field, ty) <- fields
    fname <- ty.clo match
      case Fin(set) if set.size == 1 => Some(set.head)
      case _                         => None
  } yield field -> fname).toMap
}
object TyDecl extends Parser.From(Parser.tyDecl)
