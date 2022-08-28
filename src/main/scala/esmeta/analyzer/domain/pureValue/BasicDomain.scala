package esmeta.analyzer.domain.pureValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

/** basic domain for pure values */
object BasicDomain extends pureValue.Domain {

  /** elements */
  case class Elem(
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    part: AbsPart = AbsPart.Bot,
    astValue: AbsAstValue = AbsAstValue.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
  ) extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract pure value")

  /** bottom element */
  val Bot: Elem = Elem()

  /** abstraction functions */
  def alpha(xs: Iterable[APureValue]): Elem = Elem(
    AbsClo(xs.collect { case x: AClo => x }),
    AbsCont(xs.collect { case x: ACont => x }),
    AbsPart(xs.collect { case x: Part => x }),
    AbsAstValue(xs.collect { case x: AstValue => x }),
    AbsGrammar(xs.collect { case x: Grammar => x }),
    AbsCodeUnit(xs.collect { case x: CodeUnit => x }),
    AbsConst(xs.collect { case x: Const => x }),
    AbsMath(xs.collect { case x: Math => x }),
    AbsSimpleValue(xs.collect { case x: SimpleValue => x }),
  )

  /** predefined top values */
  val cloTop: Elem = Bot.copy(clo = AbsClo.Top)
  val contTop: Elem = Bot.copy(cont = AbsCont.Top)
  val partTop: Elem = Bot.copy(part = AbsPart.Top)
  val astValueTop: Elem = Bot.copy(astValue = AbsAstValue.Top)
  val grammarTop: Elem = Bot.copy(grammar = AbsGrammar.Top)
  val codeUnitTop: Elem = Bot.copy(codeUnit = AbsCodeUnit.Top)
  val constTop: Elem = Bot.copy(const = AbsConst.Top)
  val mathTop: Elem = Bot.copy(math = AbsMath.Top)
  val simpleValueTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.Top)
  val numberTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.numberTop)
  val bigIntTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.bigIntTop)
  val strTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.strTop)
  val boolTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.boolTop)
  val undefTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.undefTop)
  val nullTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.nullTop)
  val absentTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.absentTop)

  /** constructors */
  def apply(
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    part: AbsPart = AbsPart.Bot,
    astValue: AbsAstValue = AbsAstValue.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
    num: AbsNumber = AbsNumber.Bot,
    bigInt: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = Elem(
    clo,
    cont,
    part,
    astValue,
    grammar,
    codeUnit,
    const,
    math,
    simpleValue ⊔ AbsSimpleValue(num, bigInt, str, bool, undef, nullv, absent),
  )

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = Some(
    (
      elem.clo,
      elem.cont,
      elem.part,
      elem.astValue,
      elem.grammar,
      elem.codeUnit,
      elem.const,
      elem.math,
      elem.simpleValue,
    ),
  )

  /** appender */
  given rule: Rule[Elem] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      var strs = Vector[String]()
      if (!elem.clo.isBottom) strs :+= elem.clo.toString
      if (!elem.cont.isBottom) strs :+= elem.cont.toString
      if (!elem.part.isBottom) strs :+= elem.part.toString
      if (!elem.astValue.isBottom) strs :+= elem.astValue.toString
      if (!elem.grammar.isBottom) strs :+= elem.grammar.toString
      if (!elem.codeUnit.isBottom) strs :+= elem.codeUnit.toString
      if (!elem.const.isBottom) strs :+= elem.const.toString
      if (!elem.math.isBottom) strs :+= elem.math.toString
      if (!elem.simpleValue.isBottom) strs :+= elem.simpleValue.toString
      app >> strs.mkString(", ")
    }
  }

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean =
      elem.clo ⊑ that.clo &&
      elem.cont ⊑ that.cont &&
      elem.part ⊑ that.part &&
      elem.astValue ⊑ that.astValue &&
      elem.grammar ⊑ that.grammar &&
      elem.codeUnit ⊑ that.codeUnit &&
      elem.const ⊑ that.const &&
      elem.math ⊑ that.math &&
      elem.simpleValue ⊑ that.simpleValue

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(
      elem.clo ⊔ that.clo,
      elem.cont ⊔ that.cont,
      elem.part ⊔ that.part,
      elem.astValue ⊔ that.astValue,
      elem.grammar ⊔ that.grammar,
      elem.codeUnit ⊔ that.codeUnit,
      elem.const ⊔ that.const,
      elem.math ⊔ that.math,
      elem.simpleValue ⊔ that.simpleValue,
    )

    /** meet operator */
    override def ⊓(that: Elem): Elem = Elem(
      elem.clo ⊓ that.clo,
      elem.cont ⊓ that.cont,
      elem.part ⊓ that.part,
      elem.astValue ⊓ that.astValue,
      elem.grammar ⊓ that.grammar,
      elem.codeUnit ⊓ that.codeUnit,
      elem.const ⊓ that.const,
      elem.math ⊓ that.math,
      elem.simpleValue ⊓ that.simpleValue,
    )

    /** prune operator */
    override def --(that: Elem): Elem = Elem(
      elem.clo -- that.clo,
      elem.cont -- that.cont,
      elem.part -- that.part,
      elem.astValue -- that.astValue,
      elem.grammar -- that.grammar,
      elem.codeUnit -- that.codeUnit,
      elem.const -- that.const,
      elem.math -- that.math,
      elem.simpleValue -- that.simpleValue,
    )

    /** concretization function */
    override def gamma: BSet[APureValue] =
      (elem.clo.gamma: BSet[APureValue]) ⊔
      elem.cont.gamma ⊔
      elem.part.gamma ⊔
      elem.astValue.gamma ⊔
      elem.grammar.gamma ⊔
      elem.codeUnit.gamma ⊔
      elem.const.gamma ⊔
      elem.math.gamma ⊔
      elem.simpleValue.gamma

    /** get single value */
    override def getSingle: Flat[APureValue] =
      (elem.clo.getSingle: Flat[APureValue]) ⊔
      elem.cont.getSingle ⊔
      elem.part.getSingle ⊔
      elem.astValue.getSingle ⊔
      elem.grammar.getSingle ⊔
      elem.codeUnit.getSingle ⊔
      elem.const.getSingle ⊔
      elem.math.getSingle ⊔
      elem.simpleValue.getSingle

    /** getters */
    def clo: AbsClo = elem.clo
    def cont: AbsCont = elem.cont
    def part: AbsPart = elem.part
    def astValue: AbsAstValue = elem.astValue
    def grammar: AbsGrammar = elem.grammar
    def codeUnit: AbsCodeUnit = elem.codeUnit
    def const: AbsConst = elem.const
    def math: AbsMath = elem.math
    def simpleValue: AbsSimpleValue = elem.simpleValue
    def number: AbsNumber = elem.simpleValue.number
    def bigInt: AbsBigInt = elem.simpleValue.bigInt
    def str: AbsStr = elem.simpleValue.str
    def bool: AbsBool = elem.simpleValue.bool
    def undef: AbsUndef = elem.simpleValue.undef
    def nullv: AbsNull = elem.simpleValue.nullv
    def absent: AbsAbsent = elem.simpleValue.absent

    /** get reachable address partitions */
    def reachableParts: Set[Part] =
      var parts = part.toSet
      for {
        AClo(_, captured) <- clo
        (_, value) <- captured
      } parts ++= value.reachableParts
      for {
        ACont(_, captured) <- cont
        (_, value) <- captured
      } parts ++= value.reachableParts
      parts
  }
}
