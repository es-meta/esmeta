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

  val getUpperMethodMap: String => MethodMap = cached { tname =>
    getParent(tname).fold(Map())(getUpperMethodMap) ++
    getMethodMap(tname)
  }

  /** get method map */
  val getMethodMap: String => MethodMap = cached { tname =>
    declMap
      .get(tname)
      .fold(Map())(_.elems.collect {
        case AbsMethod(name) => name -> (None, false)
        case ConMethod(name, optional, targetOpt) =>
          name -> (targetOpt.orElse(Some(s"Record[$tname].$name")), optional)
      }.toMap)
  }

  /** all possible methods of a base type */
  val getBaseMethods: String => Map[String, Set[String]] = cached {
    case tname =>
      val base = getBase(tname)
      val pairs = for {
        x <- getStrictSubTys(base) + base
        case (field, (Some(name), _)) <- getMethodMap(x)
      } yield field -> name
      pairs.groupMap(_._1)(_._2)
  }

  /** get all methods as field map */
  val getMethods: String => FieldMap = cached { tname =>
    import FieldMap.Elem
    val methods = getBaseMethods(tname)
    val map = getMethodMap(tname).map {
      case (field, (Some(name), opt)) => field -> Elem(CloT(name), opt, opt)
      case (field, (None, opt)) =>
        field -> Elem(CloT(methods.getOrElse(field, Set())), opt, opt)
    }
    FieldMap(
      map = getDirectSubTys(tname).foldLeft(map) { (acc, sub) =>
        getMethods(sub).map.foldLeft(acc) {
          case (acc, (x, r)) => acc.get(x).fold(acc)(l => acc + (x -> (l || r)))
        }
      },
      default = Elem.Top,
    )
  }

  /** get field type map */
  val getFieldMap: String => FieldMap = cached { tname =>
    import FieldMap.Elem
    FieldMap(
      map = getMethods(tname).map ++ declMap
        .get(tname)
        .fold(Map())(_.elems.collect {
          case Field(name, opt, typeStr) =>
            name -> Elem(ValueTy.from(typeStr), opt, opt)
        })
        .toMap,
      default = Elem.Top,
    )
  }

  /** get all field type map */
  val getAllFieldMap: String => FieldMap = cached { tname =>
    import FieldMap.Elem
    FieldMap(
      map = getParent(tname).fold(Map())(getAllFieldMap(_).map) ++
        getFieldMap(tname).map,
      default = Elem.Top,
    )
  }

  /** get diff field type map */
  val getDiffFieldMap: ((String, String)) => Option[FieldMap] =
    cached { (l, r) =>
      import FieldMap.Elem
      if (l == r) Some(FieldMap.Top)
      else if (isStrictSubTy(r, l))
        val parent = getParent(r).get
        val upper = getDiffFieldMap(l, parent).get.map
        Some(FieldMap(upper ++ getFieldMap(r).map, Elem.Top))
      else None
    }

  /** get field type */
  def getField(tname: String, f: String): FieldMap.Elem =
    getAllFieldMap(tname)(f)

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

  /** check if a type is a base type */
  val isBase: String => Boolean = cached(x => getBase(x) == x)

  /** get ancestor types */
  val getAncestors: String => List[String] = cached { tname =>
    tname :: (for {
      decl <- declMap.get(tname)
      parent <- decl.parent
    } yield getAncestors(parent)).getOrElse(Nil)
  }

  /** check if a type is a strict (proper) subtype of another */
  def isStrictSubTy(
    lmap: Map[String, FieldMap],
    rmap: Map[String, FieldMap],
  ): Boolean = lmap.forall(isStrictSubTy(_, rmap))
  def isStrictSubTy(
    lpair: (String, FieldMap),
    rmap: Map[String, FieldMap],
  ): Boolean = rmap.exists(isStrictSubTy(lpair, _))
  def isStrictSubTy(
    lpair: (String, FieldMap),
    rpair: (String, FieldMap),
  ): Boolean = lpair != rpair && isSubTy(lpair, rpair)
  def isStrictSubTy(l: String, r: String): Boolean =
    getStrictSubTys(r).contains(l)
  def isStrictSubTy(l: String, rs: Set[String]): Boolean =
    rs.exists(isStrictSubTy(l, _))
  def isStrictSubTy(ls: Set[String], r: String): Boolean =
    ls.forall(isStrictSubTy(_, r))
  def isStrictSubTy(ls: Set[String], rs: Set[String]): Boolean =
    ls.forall(isStrictSubTy(_, rs))

  /** check if a type is a subtype of another */
  def isSubTy(
    lmap: Map[String, FieldMap],
    rmap: Map[String, FieldMap],
  ): Boolean = lmap.forall(isSubTy(_, rmap))
  def isSubTy(lpair: (String, FieldMap), rmap: Map[String, FieldMap]): Boolean =
    rmap.exists(isSubTy(lpair, _))
  def isSubTy(lpair: (String, FieldMap), rpair: (String, FieldMap)): Boolean =
    val (l, lfm) = lpair
    val (r, rfm) = rpair
    (for {
      lca <- getLCA(l, r)
      ldfm <- getDiffFieldMap(lca, l)
      rdfm <- getDiffFieldMap(lca, r)
    } yield (ldfm && lfm) <= (rdfm && rfm)).getOrElse(false)
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
