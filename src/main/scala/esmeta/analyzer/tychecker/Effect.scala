package esmeta.analyzer.tychecker

import esmeta.util.Appender.*
import esmeta.ty.*
import esmeta.util.*

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

    def +(baseTy: String, field: Set[String]) = Effect(map + (baseTy -> field))

    def nonEmpty: Boolean = map.nonEmpty

    private def apply(rec: RecordTy): RecordTy = rec match
      case RecordTy.Top => rec // unsound here
      case RecordTy.Elem(rmap) => {
        RecordTy(rmap.map((nty, fm) => {
          val base = ManualInfo.tyModel.baseOf(nty)
          val kfield = map.getOrElse(base, Set.empty)
          nty -> fm.kill(kfield)
        }))
      }
    def apply(ty: ValueTy): ValueTy =
      ty.copied(record = apply(ty.record))
  }
  object Effect extends DomainLike[Effect] {

    /** top element */
    lazy val Top: Effect = exploded("Top of an Effect")

    /** bottom element */
    lazy val Bot: Effect = Effect(Map.empty)

    lazy val Empty: Effect = Effect()

    /** appender */
    private given Rule[Set[String]] = iterableRule("(", ", ", ")")
    private given Rule[Map[String, Set[String]]] = sortedMapRule(sep = " -> ")
    given rule: Rule[Effect] = (app, elem) => app >> elem.map
  }
}
