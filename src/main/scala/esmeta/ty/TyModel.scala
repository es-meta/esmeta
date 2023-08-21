package esmeta.ty

import esmeta.MANUALS_DIR
import esmeta.ty.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.annotation.tailrec

/** type modeling */
// TODO consider refactoring
case class TyModel(infos: Map[String, TyInfo] = Map()) {

  /** merge two type models */
  def ++(that: TyModel): TyModel = TyModel(this.infos ++ that.infos)

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, info) <- infos
      parent <- info.parent
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
    infos.collect { case (name, TyInfo(None, _, _)) => aux(name) }
    descs
  }
  def isSubTy(l: String, r: String): Boolean =
    (l == r) || subTys.get(r).fold(false)(_ contains l)
  def isSubTy(l: String, rset: Set[String]): Boolean =
    rset.exists(r => isSubTy(l, r))
  def isSubTy(l: String, rset: BSet[String]): Boolean = rset match
    case Inf       => true
    case Fin(rset) => isSubTy(l, rset)
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

  /** property map alias */
  type PropMap = Map[String, ValueTy]

  /** get types of all properties */
  val getAllProp: String => ValueTy = cached(tname =>
    if (tname == "IntrinsicsRecord") ObjectT
    else propMap.getOrElse(tname, Map()).values.foldLeft(BotT)(_ || _),
  )

  /** get types of property */
  def getProp(tname: String, p: String): ValueTy =
    getPropOrElse(tname, p)(AbsentT)

  /** get types of property */
  def getPropOrElse(tname: String, p: String)(default: => ValueTy): ValueTy =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      // TODO more precise for %ThrowTypeError% which is function object
      // (https://tc39.es/ecma262/2022/#table-well-known-intrinsic-objects)
      ObjectT
    else
      propMap
        .getOrElse(tname, Map())
        .getOrElse(p, default)

  /** property type */
  private lazy val propMap: Map[String, PropMap] = (for {
    name <- infos.keySet
  } yield name -> getPropMap(name)).toMap

  /** get property map */
  private def getPropMap(name: String): PropMap =
    val upper = getUpperPropMap(name)
    val lower = getLowerPropMap(name)
    lower.foldLeft(upper) {
      case (map, (k, t)) =>
        val newT = t || map.getOrElse(k, BotT)
        map + (k -> newT)
    }

  /** get property map from ancestors */
  private def getUpperPropMap(name: String): PropMap = infos.get(name) match
    case Some(info) =>
      val parentProps = info.parent.map(getUpperPropMap).getOrElse(Map())
      val props = info.props
      weakMerge(parentProps, props)
    case None => Map()

  /** get property map of name */
  private def getSamePropMap(name: String): PropMap =
    infos.get(name).map(_.props).getOrElse(Map())

  /** get property map from ancestors */
  private def getLowerPropMap(name: String): PropMap =
    directSubTys.get(name) match
      case Some(children) =>
        children
          .map(child => {
            val lower = getLowerPropMap(child)
            val props = getSamePropMap(child)
            weakMerge(lower, props)
          })
          .reduce(parallelWeakMerge)
      case None => getSamePropMap(name)

  /** weak merge */
  private def weakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
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
  private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
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
object TyModel {
  lazy val es: TyModel = ManualInfo.getTyModel()
}

/** type information */
case class TyInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  fields: Map[String, String] = Map(),
) {
  lazy val props: Map[String, ValueTy] =
    val keys = methods.keySet ++ fields.keySet
    (for {
      k <- keys
      fs = fields.get(k).fold(BotT)(ValueTy.from)
      tys = methods.get(k) match
        case None         => fs
        case Some(method) => fs || CloT(method)
    } yield k -> tys).toMap
}
