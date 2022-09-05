package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.ir.{COp, Name, VOp}
import esmeta.util.*

/** abstract valude domain */
trait Domain extends domain.Domain[AValue] {

  /** abstraction functions for an original value */
  def apply(value: Value): Elem = alpha(AValue.from(value))

  /** constructor with types */
  def apply(ty: Ty): Elem

  /** constructor for completions */
  def createCompletion(
    ty: AbsValue,
    value: AbsValue,
    target: AbsValue,
  ): Elem

  /** abstraction functions for raw data */
  def apply(ast: Ast): Elem = apply(AstValue(ast))
  def apply(n: Double): Elem = apply(Number(n))
  def apply(s: String): Elem = apply(Str(s))
  def apply(b: Boolean): Elem = apply(Bool(b))
  def apply(d: BigDecimal): Elem = apply(Math(d))

  /** helpers for make transition for variadic operators */
  protected def doVopTransfer[T](
    f: Elem => Option[T],
    op: (T, T) => T,
    g: T => Elem,
    vs: List[Elem],
  ): Elem =
    val vst = vs.map(f).flatten
    if (vst.size != vs.size) Bot
    else g(vst.reduce(op))

  /** predefined top values */
  def compTop: Elem
  def pureValueTop: Elem
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
    comp: AbsComp = AbsComp.Bot,
    pureValue: AbsPureValue = AbsPureValue.Bot,
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
    AbsComp,
    AbsPureValue,
  )

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple]

  /** transfer for variadic operation */
  def vopTransfer(vop: VOp, vs: List[Elem]): Elem

  /** abstract value interfaces */
  extension (elem: Elem) {

    /** get key values */
    def keyValue: Elem

    /** bitwise operations */
    def &(that: Elem): Elem
    def |(that: Elem): Elem
    def ^(that: Elem): Elem

    /** comparison operations */
    def =^=(that: Elem): Elem
    def ==^==(that: Elem): Elem
    def <(that: Elem): Elem

    /** logical operations */
    def &&(that: Elem): Elem
    def ||(that: Elem): Elem
    def ^^(that: Elem): Elem

    /** numeric operations */
    def +(that: Elem): Elem
    def sub(that: Elem): Elem
    def /(that: Elem): Elem
    def *(that: Elem): Elem
    def %(that: Elem): Elem
    def %%(that: Elem): Elem
    def **(that: Elem): Elem
    def <<(that: Elem): Elem
    def >>>(that: Elem): Elem
    def >>(that: Elem): Elem

    /** unary operations */
    def unary_- : Elem
    def unary_! : Elem
    def unary_~ : Elem
    def abs: Elem
    def floor: Elem

    /** type operations */
    def typeOf(st: AbsState): Elem
    def typeCheck(tname: String, st: AbsState): Elem

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: Elem): Elem
    def sourceText: Elem
    def parse(rule: Elem): Elem
    def duplicated(st: AbsState): Elem
    def substring(from: Elem): Elem
    def substring(from: Elem, to: Elem): Elem
    def clamp(lower: Elem, upper: Elem): Elem
    def isArrayIndex: Elem

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem
    def pruneTypeCheck(tname: String, positive: Boolean): Elem
    def pruneValue(r: Elem, positive: Boolean): Elem

    /** single check */
    def isSingle: Boolean = elem.getSingle match
      case One(_) => true
      case _      => false

    /** get reachable address partitions */
    def reachableParts: Set[Part]

    /** completion helpers */
    def wrapCompletion: Elem
    def unwrapCompletion: Elem
    def isCompletion: Elem
    def abruptCompletion: Elem

    /** absent helpers */
    def removeAbsent: Elem
    def isAbsent: Elem

    /** refine receiver object */
    def refineThis(func: Func): Elem

    /** getters */
    def comp: AbsComp
    def pureValue: AbsPureValue
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
    def ty: ValueTy
  }
}
