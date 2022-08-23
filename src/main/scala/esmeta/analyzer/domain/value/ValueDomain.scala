package esmeta.analyzer.domain

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.ir.{COp, Name, VOp}
import esmeta.es.Ast
import esmeta.util.Appender.*

/** domain for abstract values */
trait ValueDomain extends Domain {

  /** abstraction functions */
  def apply(ast: Ast): Elem
  def apply(num: Number): Elem
  def apply(num: Double): Elem
  def apply(bigInt: BigInt): Elem
  def apply(str: String): Elem
  def apply(bool: Boolean): Elem
  def apply(d: BigDecimal): Elem
  lazy val codeunit: Elem
  lazy val math: Elem
  lazy val num: Elem
  lazy val bigInt: Elem
  lazy val str: Elem
  lazy val bool: Elem
  lazy val undef: Elem
  lazy val nullv: Elem
  lazy val absent: Elem
  def apply(value: Value): Elem = this(AValue.from(value))
  def apply(value: AValue): Elem
  def apply(tys: Type*): Elem
  def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem

  /** transfer for variadic operation */
  def vopTransfer(vop: VOp, vs: List[Elem]): Elem

  /** helpers for make transition for variadic operators */
  protected def doVopTransfer[T](
    f: Elem => Option[T],
    op: (T, T) => T,
    g: T => Elem,
    vs: List[Elem],
  ): Elem = {
    val vst = vs.map(f).flatten
    if (vst.size != vs.size) Bot
    else g(vst.reduce(op))
  }

  /** elements */
  type Elem <: ValueElemTrait
  trait ValueElemTrait extends ElemTrait { this: Elem =>

    /** getters */
    def getKeyValue: Elem
    def getClos: List[(Func, Map[Name, Elem])]
    def getConts: List[ACont]
    def getSDO(method: String): List[(Func, Elem)]
    def getLexical(method: String): Elem
    def getTypedArguments: List[(Elem, Type)]

    /** meet operator */
    def ⊓(that: Elem): Elem

    /** minus operator */
    def -(that: Elem): Elem

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
    def convert(cop: COp, radix: Elem): Elem
    def sourceText: Elem
    def parse(rule: Elem): Elem
    def duplicated(st: AbsState): Elem
    def substring(from: Elem, to: Elem): Elem
    def isArrayIndex: Elem

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem
    def pruneTypeCheck(tname: String, positive: Boolean): Elem
    def pruneValue(r: Elem, positive: Boolean): Elem

    /** singleton */
    def getSingle: Flat[AValue]
    def isSingle: Boolean = getSingle match
      case FlatElem(_) => true
      case _           => false

    /** get reachable locations */
    def reachableLocs: Set[Loc]

    /** completion helpers */
    def wrapCompletion: Elem
    def unwrapCompletion: Elem // TODO warning
    def isCompletion: Elem
    def abruptCompletion: Elem

    /** absent helpers */
    def removeAbsent: Elem
    def isAbsent: Elem

    /** refine receiver object */
    def refineThis(func: Func): Elem
  }
}
