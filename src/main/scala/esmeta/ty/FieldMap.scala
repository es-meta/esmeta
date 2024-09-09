package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** field type map */
case class FieldMap(map: Map[String, FieldMap.Elem])
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
          elem = this(field) || that(field)
          if !elem.isTop
        } yield field -> elem).toMap,
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
          elem = this(field) && that(field)
          if !elem.isTop
        } yield field -> elem).toMap,
      )

  /** TODO prune type */
  def --(that: => FieldMap): FieldMap = this

  /** field accessor */
  def apply(field: String): Elem = map.getOrElse(field, Elem.Top)

  /** field update */
  def update(field: String, elem: Elem): FieldMap =
    FieldMap(map + (field -> elem))

  /** fields */
  def fields: Set[String] = map.keySet

  /** field map containment check */
  def contains(record: RecordObj, heap: Heap): Boolean =
    var fields = map.keySet
    fields.forall { f => this(f).contains(record.get(Str(f)), heap) }
}

object FieldMap extends Parser.From(Parser.fieldMap) {
  lazy val Top: FieldMap = FieldMap()

  def apply(fields: (String, Elem)*): FieldMap = FieldMap(fields.toMap)

  /** optinoal value types */
  case class Elem(
    value: ValueTy,
    uninit: Boolean = false,
    absent: Boolean = false,
  ) extends TyElem
    with Lattice[Elem] {

    /** top check */
    def isTop: Boolean = value.isTop && uninit.isTop && absent.isTop

    /** bottom check */
    def isBottom: Boolean = value.isBottom && uninit.isBottom && absent.isBottom

    /** Absent check */
    def isAbsent: Boolean = value.isBottom && !uninit && absent

    /** partial order/subset operator */
    def <=(that: => Elem): Boolean = (this eq that) || {
      this.value <= that.value &&
      this.uninit <= that.uninit &&
      this.absent <= that.absent
    }

    /** union type */
    def ||(that: => Elem): Elem =
      if (this eq that) this
      else
        Elem(
          this.value || that.value,
          this.uninit || that.uninit,
          this.absent || that.absent,
        )

    /** intersection type */
    def &&(that: => Elem): Elem =
      if (this eq that) this
      else
        Elem(
          this.value && that.value,
          this.uninit && that.uninit,
          this.absent && that.absent,
        )

    /** prune type */
    def --(that: => Elem): Elem =
      if (that.isBottom) this
      else
        Elem(
          this.value -- that.value,
          this.uninit -- that.uninit,
          this.absent -- that.absent,
        )

    /** existence check */
    def exists: Set[Boolean] =
      (if (!value.isBottom || uninit) Set(true) else Set()) ++
      (if (absent) Set(false) else Set())

    /** containment check */
    def contains(v: Option[Value | Uninit], heap: Heap): Boolean = v match
      case Some(value: Value) => this.value.contains(value, heap)
      case Some(Uninit)       => this.uninit
      case None               => this.absent
  }
  object Elem extends Parser.From(Parser.fieldMapElem) {
    lazy val Top: Elem = Elem(AnyT, true, true)
    lazy val Exist: Elem = Elem(AnyT, true, false)
    lazy val Init: Elem = Elem(AnyT, false, false)
    lazy val Uninit: Elem = Elem(BotT, true, false)
    lazy val Absent: Elem = Elem(BotT, false, true)
    lazy val Bot: Elem = Elem(BotT, false, false)
  }
}
