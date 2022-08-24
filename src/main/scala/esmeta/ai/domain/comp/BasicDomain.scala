package esmeta.ai.domain.comp

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** basic domain for completion records */
class BasicDomain(val config: Config) extends comp.Domain {
  import config.*

  /** elements */
  case class Elem(map: Map[String, Result] = Map())

  /** top element */
  lazy val Top = exploded("top abstract completion record")

  /** bottom element */
  val Bot = Elem()

  /** results in completion records */
  case class Result(value: AbsPureValue, target: AbsOptStr) {
    def isBottom = value.isBottom && target.isBottom
    def ⊑(that: Result): Boolean =
      this.value ⊑ that.value && this.target ⊑ that.target
    def ⊔(that: Result): Result =
      Result(this.value ⊔ that.value, this.target ⊔ that.target)
    def ⊓(that: Result): Result =
      Result(this.value ⊓ that.value, this.target ⊓ that.target)
    def -(that: Result): Result =
      Result(this.value - that.value, this.target - that.target)
  }
  object Result { val Bot = Result(AbsPureValue.Bot, AbsOptStr.Bot) }

  /** abstraction functions */
  def alpha(xs: Iterable[Comp]): Elem = ??? // (for {
  //   ty <- xs.map(_.ty.name)
  //   values = xs .filter(_.ty.name == ty) .map(_.value)
  //   targets = xs .filter(_.ty.name == ty) .map(_.target)
  // } yield ty -> Result(AbsPureValue(value), AbsOptStr(target))).toMap

  /** appender */
  given rule: Rule[Elem] = ??? // (app, elem) => {
  //   app >> elem.map.toList
  //     .sortBy(_._1)
  //     .map { case (k, Result(v, t)) => s"~$k~ -> ($v, $t)" }
  //     .mkString("{", ", ", "}")

  // // constructors
  // def apply(pairs: (String, Result)*): Elem = this(pairs.toMap)
  // def apply(map: Map[String, Result]): Elem = Elem(map)
  // def unapply(elem: Elem) = Some(elem.map)

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = ??? // (this, that) match
    //   case _ if this.isBottom => true
    //   case _ if that.isBottom => false
    //   case (Elem(lmap), Elem(rmap)) =>
    //     (lmap.keySet ++ rmap.keySet).forall(ty => {
    //       this.resultOf(ty) ⊑ that.resultOf(ty)
    //     })

    /** join operator */
    def ⊔(that: Elem): Elem = ??? // (this, that) match
    //   case _ if this.isBottom => that
    //   case _ if that.isBottom => this
    //   case (Elem(lmap), Elem(rmap)) =>
    //     val newMap = (lmap.keySet ++ rmap.keySet).toList
    //       .map(ty => {
    //         ty -> this.resultOf(ty) ⊔ that.resultOf(ty)
    //       })
    //       .toMap
    //     Elem(newMap)

    /** meet operator */
    override def ⊓(that: Elem): Elem = ??? // (this, that) match
    //   case _ if this.isBottom || that.isBottom => Bot
    //   case (Elem(lmap), Elem(rmap)) =>
    //     val newPairs = (lmap.keySet ++ rmap.keySet).toList
    //       .map(ty => {
    //         ty -> this.resultOf(ty) ⊓ that.resultOf(ty)
    //       })
    //       .filter(!_._2.isBottom)
    //     Elem(newPairs.toMap)

    /** prune operator */
    override def -(that: Elem): Elem = ??? // (this, that) match
    //   case _ if this ⊑ that            => Bot
    //   case _ if (this ⊓ that).isBottom => this
    //   case (Elem(lmap), Elem(rmap)) =>
    //     val newPairs = for {
    //       (ty, lres) <- lmap
    //       rres = that.resultOf(ty)
    //       newRes = lres - rres if !newRes.isBottom
    //     } yield ty -> newRes
    //     Elem(newPairs.toMap)

    // // normal completions
    // def normal: Result = resultOf("normal")

    // // remove normal completions
    // def removeNormal: Elem = copy(map = map - "normal")

    // // result of each completion type
    // def resultOf(ty: String): Result =
    //   map.getOrElse(ty, Result(AbsPureValue.Bot, AbsPureValue.Bot))

    // // get single value
    // def getSingle: Flat[AComp] =
    //   if (isBottom) FlatBot
    //   else
    //     map.toList match
    //       case List((ty, Result(value, target))) =>
    //         (value.getSingle, target.getSingle) match {
    //           case (FlatElem(v), FlatElem(t)) =>
    //             FlatElem(AComp(AConst(ty), v, t))
    //           case _ => FlatTop
    //         }
    //       case _ => FlatTop

    // // merged result
    // def mergedResult: Result =
    //   map.map { case (k, v) => v }.foldLeft(Result.Bot)(_ ⊔ _)

    // // lookup
    // def apply(value: AbsPureValue): AbsPureValue =
    //   var newV = AbsPureValue.Bot
    //   val Result(v, t) = mergedResult
    //   if (AV_TYPE ⊑ value)
    //     newV ⊔= AbsPureValue(const = AbsConst(map.keySet.map(AConst.apply)))
    //   if (AV_VALUE ⊑ value) newV ⊔= v
    //   if (AV_TARGET ⊑ value) newV ⊔= t
    //   newV
  }
}
