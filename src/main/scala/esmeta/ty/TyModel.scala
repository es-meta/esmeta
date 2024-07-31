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
  val getFieldMap: String => FieldMap =
    cached { declMap.get(_).fold(FieldMap.Top)(_.fieldMap) }

  val getAllFieldMap: String => FieldMap = cached { tname =>
    FieldMap(
      getParent(tname).fold(Map())(getAllFieldMap(_).map) ++
      getFieldMap(tname).map,
    )
  }

  /** get diff field type map */
  val getDiffFieldMap: ((String, String)) => Option[FieldMap] =
    cached { (l, r) =>
      if (l == r) Some(FieldMap.Top)
      else if (isStrictSubTy(l, r))
        val upper = getDiffFieldMap(getParent(l).get, r).get.map
        Some(FieldMap(upper ++ getFieldMap(l).map))
      else None
    }

  /** get field type */
  def getField(tname: String, p: String): ValueTy = getAllFieldMap(tname)(p)

  /** get least common ancestor */
  val getLCA: ((String, String)) => Option[String] = cached { (l, r) =>
    val lances = getAncestors(l)
    val rances = getAncestors(r)
    if (isSubTy(l, r)) Some(r)
    else if (isSubTy(r, l)) Some(l)
    else
      (getParent(l), getParent(r)) match
        case (Some(lp), Some(rp)) => getLCA(lp, rp)
        case _                    => None
  }

  /** get parent type name */
  val getParent: String => Option[String] =
    cached(declMap.get(_).flatMap(_.parent))

  /** get base type name */
  val getBase: String => String = cached(getAncestors(_).last)

  /** get ancestor types */
  val getAncestors: String => List[String] = cached { tname =>
    tname :: (for {
      decl <- declMap.get(tname)
      parent <- decl.parent
    } yield getAncestors(parent)).getOrElse(Nil)
  }

  /** get methods */
  val getMethod: String => MethodMap = cached { tname =>
    (for {
      (field, ty) <- getAllFieldMap(tname).map
      fname <- ty.clo match
        case Fin(set) if !ty.absent && set.size == 1 => Some(set.head)
        case _                                       => None
    } yield field -> fname).toMap
  }

  /** check if a type is a strict (proper) subtype of another */
  def isStrictSubTy(l: String, r: String): Boolean =
    getStrictSubTys(r).contains(l)
  def isStrictSubTy(l: String, rs: Set[String]): Boolean =
    rs.exists(isStrictSubTy(l, _))
  def isStrictSubTy(ls: Set[String], r: String): Boolean =
    ls.forall(isStrictSubTy(_, r))
  def isStrictSubTy(ls: Set[String], rs: Set[String]): Boolean =
    ls.forall(isStrictSubTy(_, rs))

  /** check if a type is a subtype of another */
  def isSubTy(l: String, r: String): Boolean =
    l == r || isStrictSubTy(l, r)

  /** strict subtypes */
  lazy val getStrictSubTys: String => Set[String] = cached { tname =>
    for {
      sub <- directSubTys.getOrElse(tname, Set())
      name <- getStrictSubTys(sub) + sub
    } yield name
  }

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
