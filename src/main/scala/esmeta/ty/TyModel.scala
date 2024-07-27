package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** type modeling */
// TODO consider refactoring
case class TyModel(decls: Map[String, TyDecl] = Map()) extends TyElem {

  /** merge two type models */
  def ++(that: TyModel): TyModel = TyModel(this.decls ++ that.decls)

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] =
    decls.get(tname) match {
      case Some(decl) =>
        val parentMethods = decl.parent.map(getMethod).getOrElse(Map())
        parentMethods ++ decl.methods
      case None => Map()
    }

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, decl) <- decls
      parent <- decl.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + name)
    children
  }

  /** subtypes */
  lazy val subTys: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- directSubTys.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    decls.collect { case (name, TyDecl(_, None, _)) => aux(name) }
    descs
  }
  def isSubTy(l: String, r: String): Boolean =
    (l == r) || subTys.get(r).fold(false)(_ contains l)
  def isSubTy(l: String, rset: Set[String]): Boolean =
    rset.exists(r => isSubTy(l, r))
  def isSubTy(l: String, rset: BSet[String]): Boolean = rset match
    case Inf       => true
    case Fin(rset) => isSubTy(l, rset)
  def isSubTy(lset: Set[String], r: String): Boolean =
    lset.forall(l => isSubTy(l, r))
  def isSubTy(lset: Set[String], rset: Set[String]): Boolean =
    lset.forall(l => isSubTy(l, rset))

  /** loose subtyping relation between two value types */
  def isLooseSubTy(
    l: ValueTy,
    r: ValueTy,
  ): Boolean =
    val pureValue = isLooseSubTy(l.pureValue, r.pureValue)
    val normal = isLooseSubTy(l.normal, r.normal)
    val abrupt = l.abrupt <= r.abrupt
    pureValue && normal && abrupt

  /** loose subtyping relation between two pure value types */
  def isLooseSubTy(
    l: PureValueTy,
    r: PureValueTy,
  ): Boolean =
    val noName =
      ((l -- NameT.pureValue) <= (r -- NameT.pureValue))
    val name = ((l.name.set, r.name.set) match
      case (_, Inf) => true
      case (Inf, _) => false
      case (Fin(lset), Fin(rset)) =>
        lset.forall(l => rset.exists(r => isSubTy(l, r) || isSubTy(r, l)))
    )
    noName && name

  /** field map alias */
  type FieldMap = Map[String, ValueTy]

  /** get types of field */
  def getField(tname: String, p: String): ValueTy =
    fieldMaps.getOrElse(tname, Map()).getOrElse(p, AnyT)

  /** get field map */
  def getFieldMap(name: String): FieldMap =
    val upper = getUpperFieldMap(name)
    val lower = getLowerFieldMap(name)
    lower.foldLeft(upper) {
      case (map, (k, t)) =>
        val newT = t || map.getOrElse(k, BotT)
        map + (k -> newT)
    }

  /** field type */
  private lazy val fieldMaps: Map[String, FieldMap] = (for {
    name <- decls.keySet
  } yield name -> getFieldMap(name)).toMap

  /** get field map from ancestors */
  private def getUpperFieldMap(name: String): FieldMap = decls.get(name) match
    case Some(decl) =>
      val parentfields = decl.parent.map(getUpperFieldMap).getOrElse(Map())
      val fields = decl.fields
      weakMerge(parentfields, fields)
    case None => Map()

  /** get field map of name */
  private def getSameFieldMap(name: String): FieldMap =
    decls.get(name).map(_.fields).getOrElse(Map())

  /** get subtypes with field existence */
  lazy val getSubTypes: ((String, String)) => List[String] =
    cached((name, key) =>
      val exist = getSameFieldMap(name).contains(key)
      if (exist) List(name)
      else
        directSubTys
          .get(name)
          .fold(Nil)(_.flatMap(child => getSubTypes(child, key)).toList),
    )

  /** get field map from ancestors */
  private def getLowerFieldMap(name: String): FieldMap =
    directSubTys.get(name) match
      case Some(children) =>
        children
          .map(child => {
            val lower = getLowerFieldMap(child)
            val fields = getSameFieldMap(child)
            weakMerge(lower, fields)
          })
          .reduce(parallelWeakMerge)
      case None => getSameFieldMap(name)

  /** weak merge */
  private def weakMerge(lmap: FieldMap, rmap: FieldMap): FieldMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, BotT)
        val rt = rmap.getOrElse(k, BotT)
        k -> (lt || rt)
      })
      .toMap
  }

  /** parallel weak merge */
  private def parallelWeakMerge(lmap: FieldMap, rmap: FieldMap): FieldMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, AbsentT)
        val rt = rmap.getOrElse(k, AbsentT)
        k -> (lt || rt)
      })
      .toMap
  }

}
object TyModel extends Parser.From(Parser.tyModel)

/** type declrmation */
case class TyDecl(
  name: String,
  parent: Option[String] = None,
  fields: Map[String, ValueTy] = Map(),
) extends TyElem {
  def methods: Map[String, String] = (for {
    (field, ty) <- fields
    fname <- ty.clo match
      case Fin(set) if set.size == 1 => Some(set.head)
      case _                         => None
  } yield field -> fname).toMap
}
object TyDecl extends Parser.From(Parser.tyDecl)
