package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type modeling */
case class TyModel(decls: List[TyDecl] = Nil) extends TyElem {

  /** type declaration map */
  val declMap: Map[String, TyDecl] = (for {
    decl <- decls
  } yield decl.name -> decl).toMap

  /** get field type map */
  val getFieldMap: String => FieldMap = cached(tname => {
    FieldMap((for {
      decl <- declMap.get(tname)
      parentMap = decl.parent.fold(Map())(getFieldMap(_).map)
    } yield parentMap ++ decl.typeMap).getOrElse(Map()))
  })

  /** get field type */
  def getField(tname: String, p: String): ValueTy =
    getFieldMap(tname)(p)

  /** get base type name */
  val getBase: String => String = cached(tname => {
    (for {
      decl <- declMap.get(tname)
      parent <- decl.parent
    } yield getBase(parent)).getOrElse(tname)
  })

  /** get methods */
  val getMethod: String => MethodMap = cached(tname => {
    (for {
      (field, ty) <- getFieldMap(tname).map
      fname <- ty.clo match
        case Fin(set) if !ty.absent && set.size == 1 => Some(set.head)
        case _                                       => None
    } yield field -> fname).toMap
  })

  def isSubTy(l: String, r: String): Boolean =
    l == r || getSubTys(r).contains(l)
  def isSubTy(l: String, rs: Set[String]): Boolean =
    rs.exists(isSubTy(l, _))
  def isSubTy(ls: Set[String], r: String): Boolean =
    ls.forall(isSubTy(_, r))
  def isSubTy(ls: Set[String], rs: Set[String]): Boolean =
    ls.forall(isSubTy(_, rs))

  /** strict subtypes */
  lazy val getSubTys: String => Set[String] =
    cached(tname => getStrictSubTys(tname) + tname)

  /** strict subtypes */
  lazy val getStrictSubTys: String => Set[String] = cached(tname => {
    for {
      sub <- directSubTys.getOrElse(tname, Set())
      name <- getSubTys(sub)
    } yield name
  })

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      decl <- decls
      parent <- decl.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + decl.name)
    children
  }
}
object TyModel extends Parser.From(Parser.tyModel)
