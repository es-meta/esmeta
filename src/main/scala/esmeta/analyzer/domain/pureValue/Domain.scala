package esmeta.analyzer.domain.pureValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** abstract pure value (value except completion record) domain */
trait Domain extends domain.Domain[APureValue] {

  /** abstraction functions for an original pure value */
  def alpha(pureValue: PureValue): Elem = alpha(APureValue.from(pureValue))

  /** predefined top values */
  def cloTop: Elem
  def contTop: Elem
  def partTop: Elem
  def astValueTop: Elem
  def grammarTop: Elem
  def codeUnitTop: Elem
  def constTop: Elem
  def mathTop: Elem
  def simpleValueTop: Elem
  def numberTop: Elem
  def bigIntTop: Elem
  def strTop: Elem
  def boolTop: Elem
  def undefTop: Elem
  def nullTop: Elem
  def absentTop: Elem

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
  ): Elem

  /** raw tuple of each simple value type */
  type RawTuple = (
    AbsClo,
    AbsCont,
    AbsPart,
    AbsAstValue,
    AbsGrammar,
    AbsCodeUnit,
    AbsConst,
    AbsMath,
    AbsSimpleValue,
  )

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple]

  /** element interfaces */
  extension (elem: Elem) {

    /** getters */
    def clo: AbsClo
    def cont: AbsCont
    def part: AbsPart
    def astValue: AbsAstValue
    def grammar: AbsGrammar
    def codeUnit: AbsCodeUnit
    def const: AbsConst
    def math: AbsMath
    def simpleValue: AbsSimpleValue
    def number: AbsNumber
    def bigInt: AbsBigInt
    def str: AbsStr
    def bool: AbsBool
    def undef: AbsUndef
    def nullv: AbsNull
    def absent: AbsAbsent

    /** remove absent values */
    def removeAbsent: Elem = elem - absentTop

    /** get reachable address partitions */
    def reachableParts: Set[Part]
  }
}
