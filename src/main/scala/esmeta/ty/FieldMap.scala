package esmeta.ty

import esmeta.state.{RecordObj, Heap, Str}
import esmeta.ty.util.Parser
import esmeta.util.*

/** field type map */
case class FieldMap(map: Map[String, OptValueTy] = Map())
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

  /** filter fields */
  def filter(pred: String => Boolean): FieldMap =
    FieldMap(map.filter { case (field, _) => pred(field) })

  /** field accessor */
  def apply(field: String): OptValueTy = map.getOrElse(field, OptValueTy.Top)

  /** field update */
  def update(field: String, ty: ValueTy): FieldMap =
    FieldMap(map + (field -> OptValueTy(ty, false)))

  /** fields */
  def fields: Set[String] = map.keySet

  /** field map containment check */
  def contains(record: RecordObj, heap: Heap): Boolean =
    map.forall { case (f, ty) => ty.contains(record.get(Str(f)), heap) }
}

object FieldMap extends Parser.From(Parser.fieldMap) {
  lazy val Top: FieldMap = FieldMap(Map())
}
