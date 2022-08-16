package esmeta.analyzer.domain

import esmeta.state.*
import esmeta.util.Appender
import esmeta.util.Appender.*

// basic abstract completions
object BasicCompDomain extends Domain {

  // TODO refactoring
  // value domain should be basic value domain */
  lazy val AbsValue = BasicValueDomain
  type AbsValue = AbsValue.Elem

  /** bottom element */
  val Bot = Elem(Map())

  // results
  case class Result(value: AbsValue, target: AbsValue) {
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
  object Result {
    val Bot = Result(AbsValue.Bot, AbsValue.Bot)
  }

  // abstraction functions
  def apply(comp: AComp): Elem =
    val AComp(ty, value, target) = comp
    Elem(Map(ty.name -> Result(AbsValue(value), AbsValue(target))))

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    app >> elem.map.toList
      .sortBy(_._1)
      .map { case (k, Result(v, t)) => s"~$k~ -> ($v, $t)" }
      .mkString("{", ", ", "}")

  // constructors
  def apply(pairs: (String, Result)*): Elem = this(pairs.toMap)
  def apply(map: Map[String, Result]): Elem = Elem(map)
  def unapply(elem: Elem) = Some(elem.map)

  // elements
  case class Elem(map: Map[String, Result]) extends ElemTrait {
    // partial order
    override def isBottom = map.isEmpty

    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (Elem(lmap), Elem(rmap)) =>
        (lmap.keySet ++ rmap.keySet).forall(ty => {
          this.resultOf(ty) ⊑ that.resultOf(ty)
        })

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (Elem(lmap), Elem(rmap)) =>
        val newMap = (lmap.keySet ++ rmap.keySet).toList
          .map(ty => {
            ty -> this.resultOf(ty) ⊔ that.resultOf(ty)
          })
          .toMap
        Elem(newMap)

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (Elem(lmap), Elem(rmap)) =>
        val newPairs = (lmap.keySet ++ rmap.keySet).toList
          .map(ty => {
            ty -> this.resultOf(ty) ⊓ that.resultOf(ty)
          })
          .filter(!_._2.isBottom)
        Elem(newPairs.toMap)

    // minus operator
    def -(that: Elem): Elem = (this, that) match
      case _ if this ⊑ that            => Bot
      case _ if (this ⊓ that).isBottom => this
      case (Elem(lmap), Elem(rmap)) =>
        val newPairs = for {
          (ty, lres) <- lmap
          rres = that.resultOf(ty)
          newRes = lres - rres if !newRes.isBottom
        } yield ty -> newRes
        Elem(newPairs.toMap)

    // normal completions
    def normal: Result = resultOf("normal")

    // remove normal completions
    def removeNormal: Elem = copy(map = map - "normal")

    // result of each completion type
    def resultOf(ty: String): Result =
      map.getOrElse(ty, Result(AbsValue.Bot, AbsValue.Bot))

    // get single value
    def getSingle: Flat[AComp] =
      if (isBottom) FlatBot
      else
        map.toList match
          case List((ty, Result(value, target))) =>
            (value.getSingle, target.getSingle) match {
              case (FlatElem(v), FlatElem(t)) =>
                FlatElem(AComp(AConst(ty), v, t))
              case _ => FlatTop
            }
          case _ => FlatTop

    // merged result
    def mergedResult: Result =
      map.map { case (k, v) => v }.foldLeft(Result.Bot)(_ ⊔ _)

    // lookup
    def apply(value: AbsValue): AbsValue =
      var newV = AbsValue.Bot
      val Result(v, t) = mergedResult
      if (AV_TYPE ⊑ value)
        newV ⊔= AbsValue(const = AbsConst(map.keySet.map(AConst.apply)))
      if (AV_VALUE ⊑ value) newV ⊔= v
      if (AV_TARGET ⊑ value) newV ⊔= t
      newV
  }
}
