package esmeta.analyzer.domain.comp

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

trait CompBasicDomainDecl { self: Self =>

  /** basic domain for completion records */
  object CompBasicDomain extends CompDomain {

    /** elements */
    case class Elem(map: Map[String, Result] = Map()) extends Appendable

    /** top element */
    lazy val Top = exploded("top abstract completion record")

    /** bottom element */
    val Bot = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[AComp]): Elem = Elem(
      (for (ty <- xs.map(_.ty))
        yield ty.name -> Result(
          AbsPureValue(xs.filter(_.ty == ty).map(_.value)),
          AbsPureValue(xs.filter(_.ty == ty).map(_.target.fold(Absent)(Str(_)))),
        )).toMap,
    )

    /** abstraction functions */
    def alpha(ty: AbsValue, value: AbsValue, target: AbsValue): Elem =
      val v = value.pureValue
      val t = AbsPureValue(str = target.str, const = target.const)
      ty.const.gamma match
        case Inf => Top
        case Fin(set) =>
          Elem((for (ty <- set) yield ty.name -> Result(v, t)).toMap)

    /** appender */
    given rule: Rule[Elem] = (app, elem) =>
      if (!elem.isBottom)
        app >> elem.map.toList
          .sortBy { case (t, _) => t }
          .map { case (k, Result(v, t)) => s"~$k~ -> ($v, $t)" }
          .mkString("{", ", ", "}")
      else app >> "⊥"

    /** constructors with maps */
    def apply(map: Map[String, Result]): Elem = Elem(map)

    /** extractors */
    def unapply(elem: Elem): Product1[Map[String, Result]] = Tuple1(elem.map)

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean = (elem, that) match
        case _ if elem.isBottom => true
        case _ if that.isBottom => false
        case (Elem(lmap), Elem(rmap)) =>
          (lmap.keySet ++ rmap.keySet).forall(ty => {
            elem.get(ty) ⊑ that.get(ty)
          })

      /** join operator */
      def ⊔(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom => that
        case _ if that.isBottom => elem
        case (Elem(lmap), Elem(rmap)) =>
          val newMap = (lmap.keySet ++ rmap.keySet).toList
            .map(ty => ty -> elem.get(ty) ⊔ that.get(ty))
            .toMap
          Elem(newMap)

      /** meet operator */
      override def ⊓(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom || that.isBottom => Bot
        case (Elem(lmap), Elem(rmap)) =>
          val newPairs = (lmap.keySet ++ rmap.keySet).toList
            .map(ty => ty -> elem.get(ty) ⊓ that.get(ty))
            .filter { case (_, res) => !res.isBottom }
          Elem(newPairs.toMap)

      /** prune operator */
      override def --(that: Elem): Elem = (elem, that) match
        case _ if elem ⊑ that            => Bot
        case _ if (elem ⊓ that).isBottom => elem
        case (Elem(lmap), Elem(rmap)) =>
          val newPairs = for {
            (ty, lres) <- lmap
            rres = that.get(ty)
            newRes = lres -- rres if !newRes.isBottom
          } yield ty -> newRes
          Elem(newPairs.toMap)

      /** normal completions */
      def normal: Result = get("normal")

      /** remove normal completions */
      def removeNormal: Elem = elem.copy(map = elem.map - "normal")

      /** result of each completion type */
      def get(ty: String): Result =
        elem.map.getOrElse(ty, Result(AbsPureValue.Bot, AbsPureValue.Bot))

      // get single value
      override def getSingle: Flat[AComp] =
        if (!elem.isBottom) elem.map.toList match
          case List((ty, Result(value, target))) =>
            (value.getSingle, target.getSingle) match
              case (One(v), One(t)) =>
                val tOpt = t match
                  case Str(str) => Some(str)
                  case _        => None
                One(AComp(Const(ty), v, tOpt))
              case _ => Many
          case _ => Many
        else Zero

      /** merged result */
      def mergedResult: Result =
        elem.map.map { case (_, v) => v }.foldLeft(Result.Bot)(_ ⊔ _)

      /** lookup */
      def apply(str: AbsStr): AbsPureValue =
        var newV = AbsPureValue.Bot
        val Result(value, target) = mergedResult
        if (AbsStr(Str("Type")) ⊑ str)
          newV ⊔= AbsPureValue(elem.map.keySet.map(Const(_)))
        if (AbsStr(Str("Value")) ⊑ str) newV ⊔= value
        if (AbsStr(Str("Target")) ⊑ str) newV ⊔= target
        newV

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        var parts = Set[Part]()
        for ((_, Result(value, target)) <- elem.map)
          parts ++= value.reachableParts
          parts ++= target.reachableParts
        parts
    }
  }
}
