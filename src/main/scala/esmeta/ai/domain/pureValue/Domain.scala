package esmeta.ai.domain.pureValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract pure value (value except completion record) domain */
trait Domain extends domain.Domain[PureValue] {
  val config: Config
  import config._

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
  ): Elem

  /** predefined top values */
  def cloTop: Elem
  def contTop: Elem
  def addrTop: Elem
  def astValueTop: Elem
  def grammarTop: Elem
  def codeUnitTop: Elem
  def constTop: Elem
  def mathTop: Elem
  def simpleValueTop: Elem

  /** raw tuple of each simple value type */
  type RawTuple = (
    AbsClo,
    AbsCont,
    AbsAddr,
    AbsAstValue,
    AbsGrammar,
    AbsCodeUnit,
    AbsConst,
    AbsMath,
    AbsSimpleValue,
  )

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple]
}
