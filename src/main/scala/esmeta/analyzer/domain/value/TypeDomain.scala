package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.state.*
import esmeta.ir.{COp, Name, VOp}
import esmeta.parser.ESValueParser
import esmeta.util.*
import esmeta.util.Appender.*

/** type domain for values */
object TypeDomain extends value.Domain {

  /** elements */
  case class Elem() extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract value")

  /** bottom element */
  val Bot: Elem = Elem()

  /** abstraction functions */
  def alpha(xs: Iterable[AValue]): Elem = ???

  /** predefined top values */
  def compTop: Elem = ???
  def pureValueTop: Elem = ???
  val cloTop: Elem = ???
  val contTop: Elem = ???
  val partTop: Elem = ???
  val astValueTop: Elem = ???
  val grammarTop: Elem = ???
  val codeUnitTop: Elem = ???
  val constTop: Elem = ???
  val mathTop: Elem = ???
  val simpleValueTop: Elem = ???
  val numberTop: Elem = ???
  val bigIntTop: Elem = ???
  val strTop: Elem = ???
  val boolTop: Elem = ???
  val undefTop: Elem = ???
  val nullTop: Elem = ???
  val absentTop: Elem = ???

  /** constructors */
  def apply(
    comp: AbsComp = ???,
    pureValue: AbsPureValue = ???,
    clo: AbsClo = ???,
    cont: AbsCont = ???,
    part: AbsPart = ???,
    astValue: AbsAstValue = ???,
    grammar: AbsGrammar = ???,
    codeUnit: AbsCodeUnit = ???,
    const: AbsConst = ???,
    math: AbsMath = ???,
    simpleValue: AbsSimpleValue = ???,
    num: AbsNumber = ???,
    bigInt: AbsBigInt = ???,
    str: AbsStr = ???,
    bool: AbsBool = ???,
    undef: AbsUndef = ???,
    nullv: AbsNull = ???,
    absent: AbsAbsent = ???,
  ): Elem = ???

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = ???

  /** appender */
  given rule: Rule[Elem] = (app, elem) => ???

  /** transfer for variadic operation */
  def vopTransfer(vop: VOp, vs: List[Elem]): Elem = ???

  /** element interfaces */
  extension (elem: Elem) {

    /** get key values */
    def keyValue: Elem = ???

    /** partial order */
    def ⊑(that: Elem): Boolean = ???

    /** join operator */
    def ⊔(that: Elem): Elem = ???

    /** meet operator */
    override def ⊓(that: Elem): Elem = ???

    /** prune operator */
    override def --(that: Elem): Elem = ???

    /** concretization function */
    override def gamma: BSet[AValue] = ???

    /** get single value */
    override def getSingle: Flat[AValue] = ???

    /** get reachable address partitions */
    def reachableParts: Set[Part] = ???

    /** bitwise operations */
    def &(that: Elem): Elem = ???
    def |(that: Elem): Elem = ???
    def ^(that: Elem): Elem = ???

    /** comparison operations */
    def =^=(that: Elem): Elem = ???
    def ==^==(that: Elem): Elem = ???
    def <(that: Elem): Elem = ???

    /** logical operations */
    def &&(that: Elem): Elem = ???
    def ||(that: Elem): Elem = ???
    def ^^(that: Elem): Elem = ???

    /** numeric operations */
    def +(that: Elem): Elem = ???
    def sub(that: Elem): Elem = ???
    def /(that: Elem): Elem = ???
    def *(that: Elem): Elem = ???
    def %(that: Elem): Elem = ???
    def %%(that: Elem): Elem = ???
    def **(that: Elem): Elem = ???
    def <<(that: Elem): Elem = ???
    def >>>(that: Elem): Elem = ???
    def >>(that: Elem): Elem = ???

    /** unary operations */
    def unary_- : Elem = ???
    def unary_! : Elem = ???
    def unary_~ : Elem = ???
    def abs: Elem = ???
    def floor: Elem = ???

    /** type operations */
    def typeOf(st: AbsState): Elem = ???

    /** type check */
    def typeCheck(tname: String, st: AbsState): Elem = ???

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: Elem): Elem = ???
    def sourceText: Elem = ???
    def parse(rule: Elem): Elem = ???
    def duplicated(st: AbsState): Elem = ???
    def substring(from: Elem): Elem = ???
    def substring(from: Elem, to: Elem): Elem = ???
    def clamp(lower: Elem, upper: Elem): Elem = ???
    def isArrayIndex: Elem = ???

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem = ???
    def pruneTypeCheck(tname: String, positive: Boolean): Elem = ???
    def pruneValue(r: Elem, positive: Boolean): Elem = ???

    /** completion helpers */
    def wrapCompletion: Elem = ???
    def wrapCompletion(ty: String): Elem = ???
    def unwrapCompletion: Elem = ???
    def isCompletion: Elem = ???
    def abruptCompletion: Elem = ???

    /** absent helpers */
    def removeAbsent: Elem = ???
    def isAbsent: Elem = ???

    /** refine receiver object */
    def refineThis(func: Func): Elem = ???

    /** getters */
    def comp: AbsComp = ???
    def pureValue: AbsPureValue = ???
    def clo: AbsClo = ???
    def cont: AbsCont = ???
    def part: AbsPart = ???
    def astValue: AbsAstValue = ???
    def grammar: AbsGrammar = ???
    def codeUnit: AbsCodeUnit = ???
    def const: AbsConst = ???
    def math: AbsMath = ???
    def simpleValue: AbsSimpleValue = ???
    def number: AbsNumber = ???
    def bigInt: AbsBigInt = ???
    def str: AbsStr = ???
    def bool: AbsBool = ???
    def undef: AbsUndef = ???
    def nullv: AbsNull = ???
    def absent: AbsAbsent = ???
  }
}
