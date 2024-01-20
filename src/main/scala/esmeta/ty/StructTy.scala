package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*

case class StructTy(
  name: Option[String] = None,
  fields: Map[String, ValueTy] = Map(),
) {

  /** get all properties */
  lazy val props: Map[String, ValueTy] =
    val names = name.fold(Map.empty)(TyModel.es.getPropMap)
    (for {
      field <- names.keySet ++ fields.keySet
    } yield (field -> (names(field) && fields(field)))).toMap

  /** get property type */
  // TODO: cache this
  def getPropType(prop: String): ValueTy =
    val nTy = name.fold(ValueTy.Top)(TyModel.es.getProp(_, prop))
    val fTy = fields.getOrElse(prop, ValueTy.Top)
    nTy && fTy

  /** intersection type */
  def &&(that: Map[String, ValueTy]): StructTy =
    StructTy(
      None, // fix this
      (for {
        field <- (props.keySet & that.keySet)
      } yield (field -> (props(field) && that(field)))).toMap,
    )

  /** prune type */
  def --(that: Map[String, ValueTy]): StructTy =
    StructTy(
      None,
      (for {
        field <- props.keySet
      } yield (field -> (props(field) -- that(field)))).toMap,
    )

  def isSubTy(that: Map[String, ValueTy]): Boolean =
    if ((that.keySet &~ props.keySet).nonEmpty) false
    else
      (that.keySet & props.keySet).forall(field => props(field) <= that(field))

  // reconstruct name using current name and fields
  def normalized: StructTy = ???

}

/** structural types */
case class StructsTy(map: Map[String, StructTy])
  extends TyElem
  with Lattice[StructsTy] {
  import StructsTy.*
  import TyModel.es.isSubTy

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => StructsTy): Boolean = (this, that) match
    case _ if this eq that   => true
    case (_, Top) | (Bot, _) => true
    case (Top, _)            => false
    case (StructsTy(lset), StructsTy(rset)) =>
      props.forall {
        case (field, lty) =>
          val rTy = that.getPropType(field)
          lty <= rTy
      }

  /** get all properties */
  lazy val props: Map[String, ValueTy] =
    val fields = map.values.flatMap(_.props.keySet)
    fields.map(field => (field -> getPropType(field))).toMap

  /** get property type */
  // TODO: cache this
  private def getPropType(prop: String): ValueTy =
    if (map.isEmpty) ValueTy.Top
    else map.values.foldRight(ValueTy.Bot)(_.getPropType(prop) || _)

  /** union type */
  def ||(that: => StructsTy): StructsTy = (this, that) match
    case _ if this eq that                  => this
    case (_, Bot)                           => this
    case (Bot, _)                           => that
    case (Top, _) | (_, Top)                => Top
    case (StructsTy(lset), StructsTy(rset)) => StructsTy(lset ++ rset)

  /** intersection type */
  def &&(that: => StructsTy): StructsTy = (this, that) match
    case _ if this eq that   => this
    case (_, Top)            => this
    case (Top, _)            => that
    case (Bot, _) | (_, Bot) => Bot
    case (StructsTy(lmap), StructsTy(rmap)) =>
      val total = for {
        field <- (lmap.keySet ++ rmap.keySet)
      } yield (lmap
        .getOrElse(field, StructTy()) && rmap.getOrElse(field, StructTy()))

  /** prune type */
  def --(that: => StructsTy): StructsTy = (this, that) match
    case (_, Top) | (Bot, _) => Bot
    case (StructsTy(lset), _) =>
      StructsTy(lset.filter(_ isSubTy that.props))

  /** noramlized type */
  def normalized: StructsTy = ???

  /** get single value */
  def getSingle: Flat[Nothing] = ???
}
object StructsTy extends Parser.From(Parser.structsTy) {
  lazy val Top: StructsTy = StructsTy(Set(StructTy(None, Map())))
  lazy val Bot: StructsTy = StructsTy(Map())
}
