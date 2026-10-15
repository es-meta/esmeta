package esmeta.analyzer.tychecker

import esmeta.util.Appender.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.cached

/** abstract return values */
trait EffectDecl { self: TyChecker =>

  case class Effect(map: Map[String, Set[String]] = Map()) extends EffectLike {
    import Effect.*

    /** bottom check */
    def isBottom: Boolean = map.isEmpty

    /** partial order */
    def ⊑(that: Effect)(using AbsState): Boolean =
      this.map.forall {
        case (k, v) => (that.map.contains(k) && v.subsetOf(that.map(k)))
      }

    /** not partial order */
    def !⊑(that: Effect)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: Effect)(using AbsState): Effect =
      Effect(
        (for {
          k <- this.map.keySet ++ that.map.keySet
        } yield k -> (this.map.getOrElse(k, Set.empty) ++ that.map
          .getOrElse(k, Set.empty))).toMap,
      )

    /** meet operator */
    def ⊓(that: Effect)(using AbsState): Effect =
      Effect(
        (for {
          k <- this.map.keySet intersect that.map.keySet
        } yield k -> (this.map.getOrElse(k, Set.empty) intersect that.map
          .getOrElse(k, Set.empty))).toMap,
      )

    /** effect of writing a field of an object described by `baseTy` */
    def fieldUpdate(fld: String, baseTy: ValueTy)(using AbsState): Effect =
      Effect(basesOf(baseTy).map(_ -> Set(fld)).toMap) ⊔ this

    /** effect of writing an unknown field of an object described by `baseTy` */
    def allFieldsUpdate(baseTy: ValueTy)(using AbsState): Effect =
      Effect(basesOf(baseTy).map(b => b -> declaredFieldsOf(b)).toMap) ⊔ this

    /** base types of an object (every base type if unknown) */
    private def basesOf(baseTy: ValueTy): Set[String] =
      baseTy.record.bases match
        case Inf      => allBases
        case Fin(set) => set.map(ManualInfo.tyModel.baseOf(_))

    def +(baseTy: String, field: Set[String]) = Effect(map + (baseTy -> field))

    def nonEmpty: Boolean = map.nonEmpty

    private def apply(rec: RecordTy): RecordTy = rec match
      case RecordTy.Top => rec
      case RecordTy.Elem(rmap) => {
        RecordTy(rmap.map((nty, fm) => {
          val base = ManualInfo.tyModel.baseOf(nty)
          val kfield = map.getOrElse(base, Set.empty)
          val fields = fm.weaken(kfield).map.map { (field, binding) =>
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
    given rule: Rule[Effect] = (app, elem) => app >> elem.map
  }
}
