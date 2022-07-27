package esmeta.analyzer.domain

import esmeta.interp.*
import esmeta.ir.COp
import esmeta.js.Ast
import esmeta.util.Appender
import esmeta.util.Appender.*

/** domain for abstract values */
trait ValueDomain extends Domain {

  /** abstraction functions */
  def apply(ast: Ast): Elem
  def apply(num: Number): Elem
  def apply(num: Double): Elem
  def apply(bigint: BigInt): Elem
  def apply(str: String): Elem
  def apply(bool: Boolean): Elem
  def apply(d: BigDecimal): Elem
  lazy val codeunit: Elem
  lazy val math: Elem
  lazy val num: Elem
  lazy val bigint: Elem
  lazy val str: Elem
  lazy val bool: Elem
  lazy val undef: Elem
  lazy val nullv: Elem
  lazy val absent: Elem
  def apply(value: Value): Elem
  def apply(value: AValue): Elem
  def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem

  /** constructors */
  def apply(
    comp: AbsComp = AbsComp.Bot,
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    loc: AbsLoc = AbsLoc.Bot,
    ast: AbsAst = AbsAst.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeunit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simple: AbsSimple = AbsSimple.Bot,
    num: AbsNum = AbsNum.Bot,
    bigint: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem

  /** elements */
  type Elem <: ValueElemTrait
  trait ValueElemTrait extends ElemTrait { this: Elem =>

    /** getters */
    def loc: AbsLoc
    def getKeyValue: Elem
    def getDescValue: Elem
    def getClo: List[AClo]
    def getCont: List[ACont]

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

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem

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
    def isCompletion: AbsBool
    def abruptCompletion: Elem

    /** absent helpers */
    def removeAbsent: Elem
    def isAbsent: AbsBool
  }
}
