package esmeta.ai.domain.pureValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** TODO basic domain for values */
class BasicDomain(val config: Config) extends pureValue.Domain {
  import config.*

  /** TODO elements */
  case class Elem(
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    addr: AbsAddr = AbsAddr.Bot,
    astValue: AbsAstValue = AbsAstValue.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
  )

  /** top element */
  lazy val Top: Elem = exploded("top abstract pure value")

  /** bottom element */
  val Bot: Elem = Elem()

  /** abstraction functions */
  def alpha(xs: Iterable[PureValue]): Elem = Elem(
    AbsClo(xs.collect { case x: Clo => x }),
    AbsCont(xs.collect { case x: Cont => x }),
    AbsAddr(xs.collect { case x: Addr => x }),
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
  val addrTop: Elem = Bot.copy(addr = AbsAddr.Top)
  val astValueTop: Elem = Bot.copy(astValue = AbsAstValue.Top)
  val grammarTop: Elem = Bot.copy(grammar = AbsGrammar.Top)
  val codeUnitTop: Elem = Bot.copy(codeUnit = AbsCodeUnit.Top)
  val constTop: Elem = Bot.copy(const = AbsConst.Top)
  val mathTop: Elem = Bot.copy(math = AbsMath.Top)
  val simpleValueTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.Top)

  /** constructors */
  def apply(
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    addr: AbsAddr = AbsAddr.Bot,
    astValue: AbsAstValue = AbsAstValue.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
  ): Elem =
    Elem(clo, cont, addr, astValue, grammar, codeUnit, const, math, simpleValue)

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = Some(
    (
      elem.clo,
      elem.cont,
      elem.addr,
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
      if (!elem.addr.isBottom) strs :+= elem.addr.toString
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
      elem.addr ⊑ that.addr &&
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
      elem.addr ⊔ that.addr,
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
      elem.addr ⊓ that.addr,
      elem.astValue ⊓ that.astValue,
      elem.grammar ⊓ that.grammar,
      elem.codeUnit ⊓ that.codeUnit,
      elem.const ⊓ that.const,
      elem.math ⊓ that.math,
      elem.simpleValue ⊓ that.simpleValue,
    )

    /** prune operator */
    override def -(that: Elem): Elem = Elem(
      elem.clo - that.clo,
      elem.cont - that.cont,
      elem.addr - that.addr,
      elem.astValue - that.astValue,
      elem.grammar - that.grammar,
      elem.codeUnit - that.codeUnit,
      elem.const - that.const,
      elem.math - that.math,
      elem.simpleValue - that.simpleValue,
    )
  }
}
