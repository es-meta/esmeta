package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type modeling */
case class TyModel(decls: List[TyDecl] = Nil) extends TyElem {

  import TyDecl.Elem.*

  /** type declaration map */
  lazy val declMap: Map[String, TyDecl] = (for {
    decl <- decls
  } yield decl.name -> decl).toMap

  /** get method map */
  lazy val methodOf: String => Map[String, String] = cached { tname =>
    for {
      (f, binding) <- upperFieldsOf(tname)
      ty = binding.value
      if ty <= CloT && !binding.absent
      method <- ty.clo.getSingle match
        case One(method) => Some(method)
        case _           => None
    } yield f -> method
  }

  /** refiner map */
  type Refiner = Map[String, Vector[(Binding, String)]]

  /** get refiner map */
  lazy val refinerOf: String => Refiner = cached { tname =>
    childrenOf(tname)
      .collect { case d if d.isExtended => d.name }
      .map(x => x -> refinerOf(x))
      .foldLeft[Refiner](Map()) {
        case (lref, (rt, rref)) =>
          lref ++ (ownFieldsOf(rt).map((f, e) => f -> Vector(e -> rt)) ++ rref)
            .map { (f, rm) =>
              f -> lref.get(f).fold(rm) { lm =>
                lm.filter((l, _) => rm.forall((r, _) => (l && r).isBottom)) ++
                rm.filter((r, _) => lm.forall((l, _) => (l && r).isBottom))
              }
            }
            .filter((_, v) => v.nonEmpty)
      }
  }

  /** get field type */
  lazy val getField: ((String, String)) => Binding = cached { (t, f) =>
    fieldsOf(t).getOrElse(f, Binding.Absent)
  }

  /** get diff field type map */
  lazy val diffOf: ((String, String)) => Option[FieldMap] = cached { (u, l) =>
    if (u == l) Some(FieldMap.Top)
    else if (isStrictSubTy(l, u))
      val parent = parentOf(l).get
      val upper = diffOf(u, parent).get.map
      Some(FieldMap(upper ++ ownFieldsOf(l)))
    else None
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
    lmap: Map[String, FieldMap],
    rpair: (String, FieldMap),
  ): Boolean = lmap.forall(isStrictSubTy(_, rpair))
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
  def isStrictSubTy(l: String, r: String): Boolean =
    strictSubTysOf(r).contains(l)

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
    isStrictSubTy(l, r) || (for {
      lca <- lcaOf(l, r)
      ldfm <- diffOf(lca, l)
      rdfm <- diffOf(lca, r)
    } yield {
      def aux =
        rdfm.fields.forall { case f => (ldfm(f) && lfm(f)) <= rdfm(f) } &&
        rfm.fields.forall { case f => (ldfm(f) && lfm(f)) <= rfm(f) }
      aux
    }).getOrElse(false)
  def isSubTy(l: String, r: String): Boolean =
    l == r || isStrictSubTy(l, r)
  def isSubTy(l: String, rs: Set[String]): Boolean =
    rs.exists(isSubTy(l, _))
  def isSubTy(ls: Set[String], r: String): Boolean =
    ls.forall(isSubTy(_, r))
  def isSubTy(ls: Set[String], rs: Set[String]): Boolean =
    ls.forall(isSubTy(_, rs))

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

  /** get all field type map */
  lazy val fieldsOf: String => Map[String, Binding] = cached { t =>
    declMap.get(t).fold(Map()) { decl =>
      decl.parent.fold(lowerFieldsOf(t)) { (parent, extended) =>
        if (extended) upperFieldsOf(parent) ++ lowerFieldsOf(t)
        else
          ownFieldsOf(t).foldLeft(fieldsOf(parent)) {
            case (map, (f, e)) => map + (f -> (map.get(f).fold(e)(_ && e)))
          }
      }
    }
  }

  /** get direct field types */
  lazy val ownFieldsOf: String => Map[String, Binding] = cached { t =>
    declMap
      .get(t)
      .fold(Map())(_.elems.collect {
        case AbsMethod(m) => m -> Binding.Bot
        case ConMethod(m, opt, tgt) =>
          m -> Binding(CloT(tgt.getOrElse(s"Record[$t].$m")), opt, opt)
        case Field(f, opt, typeStr) =>
          f -> Binding(ValueTy.from(typeStr), opt, opt)
      })
      .toMap
  }

  /** get upper field types */
  lazy val upperFieldsOf: String => Map[String, Binding] = cached { tname =>
    parentOf(tname).fold(Map())(upperFieldsOf) ++ ownFieldsOf(tname)
  }

  /** get lower field types */
  lazy val lowerFieldsOf: String => Map[String, Binding] = cached { tname =>
    val pairs = for {
      decl <- childrenOf(tname)
      if decl.isExtended
      x = decl.name
      pair <- lowerFieldsOf(x)
    } yield pair
    pairs.foldLeft(ownFieldsOf(tname)) {
      case (fs, (f, e)) =>
        fs + (f -> (fs.getOrElse(f, Binding.Absent) || e))
    }
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
}
object TyModel extends Parser.From(Parser.tyModel)
