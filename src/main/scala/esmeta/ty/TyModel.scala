package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type modeling */
case class TyModel(decls: List[TyDecl] = Nil) extends TyElem {

  import TyDecl.Elem.*

  /** type declaration map */
  val declMap: Map[String, TyDecl] = (for {
    decl <- decls
  } yield decl.name -> decl).toMap

  type MethodMap = Map[String, (Option[String], Boolean)]

  /** get method map */
  val getMethodMap: String => MethodMap = cached { tname =>
    getParent(tname).fold(Map())(getMethodMap) ++
    declMap
      .get(tname)
      .fold(Map())(_.elems.collect {
        case AbsMethod(name) => name -> (None, false)
        case ConMethod(name, optional, targetOpt) =>
          name -> (targetOpt.orElse(Some(s"Record[$tname].$name")), optional)
      }.toMap)
  }

  /** get all methods as field map */
  val getMethods: String => FieldMap = cached { tname =>
    val base = getMethodMap(tname).map {
      case (field, (name, opt)) => field -> OptValueTy(CloT(name.toSet), opt)
    }
    FieldMap(getDirectSubTys(tname).foldLeft(base) { (acc, sub) =>
      getMethods(sub).map.foldLeft(acc) {
        case (acc, (x, v)) =>
          acc + (x -> (acc.getOrElse(x, OptValueTy.Empty) || v))
      }
    })
  }

  /** get field type map */
  val getFieldMap: String => FieldMap = cached { tname =>
    FieldMap(
      getMethods(tname).map ++ declMap
        .get(tname)
        .fold(Map())(_.elems.collect {
          case Field(name, optional, typeStr) =>
            name -> OptValueTy(ValueTy.from(typeStr), optional)
        })
        .toMap,
    )
  }

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
  def getField(tname: String, f: String): OptValueTy = getAllFieldMap(tname)(f)

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
  def isSubTy(l: String, rs: Set[String]): Boolean =
    rs.exists(isSubTy(l, _))
  def isSubTy(ls: Set[String], r: String): Boolean =
    ls.forall(isSubTy(_, r))
  def isSubTy(ls: Set[String], rs: Set[String]): Boolean =
    ls.forall(isSubTy(_, rs))

  /** get subtypes having the given field */
  lazy val getSubTysWithField: ((String, String)) => Set[String] =
    cached((name, field) => {
      if (!getField(name, field).isTop) Set(name)
      else
        for {
          sub <- directSubTys.getOrElse(name, Set())
          x <- getSubTysWithField(sub, field)
        } yield x
    })

  /** strict subtypes */
  lazy val getStrictSubTys: String => Set[String] = cached { tname =>
    for {
      sub <- directSubTys.getOrElse(tname, Set())
      name <- getStrictSubTys(sub) + sub
    } yield name
  }

  /** direct subtypes */
  lazy val getDirectSubTys: String => Set[String] = cached { tname =>
    directSubTys.getOrElse(tname, Set())
  }

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
