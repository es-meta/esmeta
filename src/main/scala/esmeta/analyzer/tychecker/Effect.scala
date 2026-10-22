package esmeta.analyzer.tychecker

import esmeta.util.Appender.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.cached

/** abstract return values */
trait EffectDecl { self: TyChecker =>

  /** `map` maps each base type to the fields that may be written, and `deleted`
    * maps it to the fields that may be deleted
    */
  case class Effect(
    map: Map[String, Set[String]] = Map(),
    deleted: Map[String, Set[String]] = Map(),
  ) extends EffectLike {
    import Effect.*

    /** bottom check */
    def isBottom: Boolean = map.isEmpty && deleted.isEmpty

    /** fields that may be written or deleted */
    def changed: Set[String] =
      (map.values.flatten ++ deleted.values.flatten).toSet

    /** partial order */
    def ⊑(that: Effect)(using AbsState): Boolean =
      leq(this.map, that.map) && leq(this.deleted, that.deleted)

    /** not partial order */
    def !⊑(that: Effect)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: Effect)(using AbsState): Effect =
      Effect(join(this.map, that.map), join(this.deleted, that.deleted))

    /** meet operator */
    def ⊓(that: Effect)(using AbsState): Effect =
      Effect(meet(this.map, that.map), meet(this.deleted, that.deleted))

    /** effect of writing a field of an object described by `baseTy` */
    def fieldUpdate(fld: String, baseTy: ValueTy)(using AbsState): Effect =
      Effect(basesOf(baseTy).map(_ -> Set(fld)).toMap) ⊔ this

    /** effect of writing an unknown field of an object described by `baseTy` */
    def allFieldsUpdate(baseTy: ValueTy)(using AbsState): Effect =
      Effect(basesOf(baseTy).map(b => b -> declaredFieldsOf(b)).toMap) ⊔ this

    /** effect of deleting a field of an object described by `baseTy` */
    def fieldDelete(fld: String, baseTy: ValueTy)(using AbsState): Effect =
      Effect(deleted = basesOf(baseTy).map(_ -> Set(fld)).toMap) ⊔ this

    /** effect of deleting an unknown field of an object described by `baseTy`
      */
    def allFieldsDelete(baseTy: ValueTy)(using AbsState): Effect =
      Effect(deleted =
        basesOf(baseTy).map(b => b -> declaredFieldsOf(b)).toMap,
      ) ⊔ this

    /** base types of an object (every base type if unknown) */
    private def basesOf(baseTy: ValueTy): Set[String] =
      baseTy.record.bases match
        case Inf      => allBases
        case Fin(set) => set.map(ManualInfo.tyModel.baseOf(_))

    def +(baseTy: String, field: Set[String]) =
      Effect(map + (baseTy -> field), deleted)

    def nonEmpty: Boolean = map.nonEmpty

    private def apply(rec: RecordTy): RecordTy = rec match
      case RecordTy.Top => rec
      case RecordTy.Elem(rmap) => {
        RecordTy(rmap.map((nty, fm) => {
          val base = ManualInfo.tyModel.baseOf(nty)
          val written = map.getOrElse(base, Set.empty)
          val fields = fm.weaken(deleted.getOrElse(base, Set.empty)).map.map {
            // a written field still exists, but its value may be any value of
            // its declared type
            case (field, binding) if written.contains(field) =>
              // an abstract method is declared without a value type
              val declared = ManualInfo.tyModel
                .fieldsOf(nty)
                .get(field)
                .map(_.value)
                .filterNot(_.isBottom)
                .getOrElse(ValueTy.Top)
              field -> binding.copy(value = binding.value || declared)
            case (field, binding) =>
              field -> binding.copy(value = apply(binding.value))
          }
          nty -> FieldMap(fields)
        }))
      }
    def apply(ty: ValueTy): ValueTy =
      if (isBottom) ty
      else
        ty.copied(
          record = apply(ty.record),
          list = ty.list.map(apply),
          map = ty.map match
            case MapTy.Elem(key, value) => MapTy(apply(key), apply(value))
            case other                  => other,
        )
  }
  object Effect extends DomainLike[Effect] {

    /** top element */
    lazy val Top: Effect = exploded("Top of an Effect")

    /** bottom element */
    lazy val Bot: Effect = Effect(Map.empty)

    lazy val Empty: Effect = Effect()

    private type FieldsMap = Map[String, Set[String]]
    private def leq(l: FieldsMap, r: FieldsMap): Boolean =
      l.forall { (k, v) => r.get(k).exists(v.subsetOf) }
    private def join(l: FieldsMap, r: FieldsMap): FieldsMap =
      (for { k <- l.keySet ++ r.keySet } yield k -> (l.getOrElse(
        k,
        Set.empty,
      ) ++ r.getOrElse(k, Set.empty))).toMap
    private def meet(l: FieldsMap, r: FieldsMap): FieldsMap =
      (for { k <- l.keySet intersect r.keySet } yield k -> (l(k) intersect r(
        k,
      ))).toMap

    /** all base types, including the one of anonymous record types */
    lazy val allBases: Set[String] =
      ManualInfo.tyModel.declMap.keySet.map(ManualInfo.tyModel.baseOf(_)) + ""

    /** all fields declared for a base type and its subtypes */
    lazy val declaredFieldsOf: String => Set[String] = cached { base =>
      val tyModel = ManualInfo.tyModel
      (tyModel.strictSubTysOf(base) + base).flatMap(
        tyModel.ownFieldsOf(_).keySet,
      )
    }

    /** any field of any object may be written */
    lazy val All: Effect =
      Effect(allBases.map(b => b -> declaredFieldsOf(b)).toMap)

    /** appender */
    private given Rule[Set[String]] = iterableRule("(", ", ", ")")
    private given Rule[Map[String, Set[String]]] = sortedMapRule(sep = " -> ")
    given rule: Rule[Effect] = (app, elem) =>
      if (elem.deleted.isEmpty) app >> elem.map
      else app >> elem.map >> " deleted " >> elem.deleted
  }
}
