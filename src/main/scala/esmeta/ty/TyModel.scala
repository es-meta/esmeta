package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type modeling */
case class TyModel(decls: List[TyDecl] = Nil) extends TyElem {

  import TyDecl.Elem.*
  import FieldMap.{Elem => FMElem}

  /** type declaration map */
  lazy val declMap: Map[String, TyDecl] = (for {
    decl <- decls
  } yield decl.name -> decl).toMap

  /** get method map */
  lazy val methodOf: String => Map[String, String] = cached { tname =>
    for {
      (f, elem) <- upperFieldsOf(tname)
      ty = elem.value
      if ty <= CloT && !elem.absent
      method <- ty.clo.getSingle match
        case One(method) => Some(method)
        case _           => None
    } yield f -> method
  }

  /** get field type */
  def getField(tname: String, f: String): FMElem = fieldMapOf(tname)(f)

  /** get all field type map */
  lazy val fieldMapOf: String => FieldMap = cached { t =>
    FieldMap(
      map = declMap.get(t).fold(Map()) { decl =>
        decl.parent.fold(lowerFieldsOf(t)) { (parent, extended) =>
          if (extended) upperFieldsOf(parent) ++ lowerFieldsOf(t)
          else
            fieldsOf(t).foldLeft(fieldMapOf(parent).map) {
              case (map, (f, e)) => map + (f -> (map.get(f).fold(e)(_ && e)))
            }
        }
      },
      default = FMElem.Absent,
    )
  }

  /** get upper field types */
  lazy val upperFieldsOf: String => Map[String, FMElem] = cached { tname =>
    parentOf(tname).fold(Map())(upperFieldsOf) ++ fieldsOf(tname)
  }

  /** get lower field types */
  lazy val lowerFieldsOf: String => Map[String, FMElem] = cached { tname =>
    val pairs = for {
      decl <- childrenOf(tname)
      if decl.isExtended
      x = decl.name
      pair <- lowerFieldsOf(x)
    } yield pair
    pairs.foldLeft(fieldsOf(tname)) {
      case (fs, (f, e)) =>
        fs + (f -> (fs.getOrElse(f, FMElem.Absent) || e))
    }
  }

  /** get direct field types */
  lazy val fieldsOf: String => Map[String, FMElem] = cached { tname =>
    declMap
      .get(tname)
      .fold(Map())(_.elems.collect {
        case AbsMethod(m) => m -> FMElem.Bot
        case ConMethod(m, opt, tgt) =>
          m -> FMElem(CloT(tgt.getOrElse(s"Record[$tname].$m")), opt, opt)
        case Field(f, opt, typeStr) =>
          f -> FMElem(ValueTy.from(typeStr), opt, opt)
      })
      .toMap
  }

  /** get diff field type map */
  lazy val diffOf: ((String, String)) => Option[FieldMap] = cached { (u, l) =>
    if (u == l) Some(FieldMap.Top)
    else if (isStrictSubTy(l, u))
      val parent = parentOf(l).get
      val upper = diffOf(u, parent).get.map
      Some(FieldMap(upper ++ fieldsOf(l), FMElem.Top))
    else None
  }

  /** check if a type is a strict (proper) subtype of another */
  def isStrictSubTy(l: String, r: String): Boolean =
    strictSubTysOf(r).contains(l)

  /** get strict subtypes */
  lazy val strictSubTysOf: String => Set[String] = cached { tname =>
    directSubTysOf(tname).foldLeft(Set[String]()) {
      case (set, name) => set ++ strictSubTysOf(name) + name
    }
  }

  /** direct subtypes */
  lazy val directSubTysOf: String => Set[String] = cached { tname =>
    childrenOf(tname).map(_.name).toSet
  }

  /** get base type name */
  lazy val baseOf: String => String = cached(ancestorsOf(_).last)

  /** get least common ancestor */
  lazy val lcaOf: ((String, String)) => Option[String] = cached { (l, r) =>
    val lances = ancestorsOf(l)
    val rances = ancestorsOf(r)
    if (isSubTy(l, r)) Some(r)
    else if (isSubTy(r, l)) Some(l)
    else
      (parentOf(l), parentOf(r)) match
        case (Some(lp), Some(rp)) => lcaOf(lp, rp)
        case _                    => None
  }

  /** get ancestor types */
  lazy val ancestorsOf: String => List[String] = cached { tname =>
    tname :: (for {
      decl <- declMap.get(tname)
      parent <- parentOf(tname)
    } yield ancestorsOf(parent)).getOrElse(Nil)
  }

  /** check if a type is a base type */
  lazy val isBase: String => Boolean = cached(x => baseOf(x) == x)

  /** get parent type name */
  lazy val parentOf: String => Option[String] = cached { tname =>
    for {
      decl <- declMap.get(tname)
      (name, _) <- decl.parent
    } yield name
  }

  /** get children type declarations */
  lazy val childrenOf: String => List[TyDecl] =
    cached { tname => children.getOrElse(tname, Nil) }
  private lazy val children: Map[String, List[TyDecl]] =
    var children = Map[String, List[TyDecl]]()
    for {
      decl <- decls
      parent <- parentOf(decl.name)
      list = children.getOrElse(parent, Nil)
    } yield children += parent -> (decl :: list)
    children.map { case (k, v) => k -> v.reverse }

  // ---------------------------------------------------------------------------
  // TODO
  // ---------------------------------------------------------------------------

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
      lca <- lcaOf(l, r)
      ldfm <- diffOf(lca, l)
      rdfm <- diffOf(lca, r)
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
  lazy val pruneField: ((String, String)) => Set[String] =
    cached((name, field) => {
      if (!getField(name, field).isTop) Set(name)
      else
        for {
          sub <- directSubTys.getOrElse(name, Set())
          x <- pruneField(sub, field)
        } yield x
    })

  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      decl <- decls
      parent <- parentOf(decl.name)
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + decl.name)
    children
  }
}
object TyModel extends Parser.From(Parser.tyModel)
