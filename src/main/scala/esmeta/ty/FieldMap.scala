package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** field type map */
case class FieldMap(map: Map[String, ValueTy] = Map())
  extends TyElem
  with Lattice[FieldMap] {

  /** top check */
  def isTop: Boolean = map.isEmpty

  /** bottom check */
  def isBottom: Boolean = false

  /** partial order/subset operator */
  def <=(that: => FieldMap): Boolean =
    (this eq that) || that.map.forall { this(_) <= _ }

  /** union type */
  def ||(that: => FieldMap): FieldMap =
    if (this eq that) this
    else
      FieldMap((for {
        field <- (this.fields intersect that.fields)
        value = this(field) || that(field)
      } yield field -> value).toMap)

  /** intersection type */
  def &&(that: => FieldMap): FieldMap =
    if (this eq that) this
    else
      FieldMap((for {
        field <- (this.fields ++ that.fields)
        value = this(field) && that(field)
      } yield field -> value).toMap)

  /** prune type */
  def --(that: => FieldMap): FieldMap = this

  /** field accessor */
  def apply(field: String): ValueTy = map.getOrElse(field, AnyT)

  /** fields */
  def fields: Set[String] = map.keySet
}

object FieldMap extends Parser.From(Parser.fieldMap) {
  lazy val Top: FieldMap = FieldMap(Map())
}
