package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** field refinement map */
case class FieldMap(map: Map[String, Binding])
  extends TyElem
  with Lattice[FieldMap] {

  import FieldMap.*

  /** top check */
  def isTop: Boolean = map.forall(_._2.isTop)

  /** bottom check */
  def isBottom: Boolean = false

  /** partial order/subset operator */
  def <=(that: => FieldMap): Boolean = (this eq that) || that.isTop || (
    that.fields.forall { f => this(f) <= that(f) },
  )

  /** union type */
  def ||(that: => FieldMap): FieldMap =
    if (this eq that) this
    else if (this.isTop || that.isTop) Top
    else
      FieldMap(
        (for {
          field <- (this.fields ++ that.fields)
          binding = this(field) || that(field)
          if !binding.isTop
        } yield field -> binding).toMap,
      )

  /** intersection type */
  def &&(that: => FieldMap): FieldMap =
    if (this eq that) this
    else if (this.isTop) that
    else if (that.isTop) this
    else
      FieldMap(
        (for {
          field <- (this.fields ++ that.fields)
          binding = this(field) && that(field)
          if !binding.isTop
        } yield field -> binding).toMap,
      )

  /** TODO prune type */
  def --(that: => FieldMap): FieldMap = this

  /** field accessor */
  def apply(field: String): Binding = map.getOrElse(field, Binding.Top)

  /** field update */
  def +(pair: (String, Binding)): FieldMap =
    val (field, binding) = pair
    FieldMap(map + (field -> binding))

  /** fields */
  def fields: Set[String] = map.keySet

  /** field map containment check */
  def contains(record: RecordObj, heap: Heap): Boolean =
    var fields = map.keySet
    fields.forall { f => this(f).contains(record.get(Str(f)), heap) }
}

object FieldMap extends Parser.From(Parser.fieldMap) {
  lazy val Top: FieldMap = FieldMap()
  def apply(fields: (String, Binding)*): FieldMap = FieldMap(fields.toMap)
  def init(fields: String*): FieldMap = FieldMap(
    fields.map(_ -> Binding.Init).toMap,
  )
}
