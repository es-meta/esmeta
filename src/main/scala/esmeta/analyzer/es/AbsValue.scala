package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: ESAnalyzer =>

  case class AbsValue() extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsValue): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsValue): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue): AbsValue = ???

    /** meet operator */
    def ⊓(that: AbsValue): AbsValue = ???

    /** get lexical result */
    def getLexical(method: String): AbsValue = ???

    /** get syntactic SDO */
    def getSdo(method: String): List[(Func, AbsValue)] = ???

    /** parse strings with a rule */
    def parse(rule: AbsValue): AbsValue = ???

    /** substring operation */
    def substring(from: AbsValue): AbsValue = ???

    /** substring operation */
    def substring(from: AbsValue, to: AbsValue): AbsValue = ???

    /** trim operation */
    def trim(isStarting: Boolean): AbsValue = ???

    /** instanceof operation */
    def instanceOf(ty: AbsValue): AbsValue = ???

    /** sizeof operation */
    def sizeOf: AbsValue = ???

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: AbsValue): AbsValue = ???

    /** bitwise operations */
    def &(that: AbsValue)(using AbsState): AbsValue = ???
    def |(that: AbsValue)(using AbsState): AbsValue = ???
    def ^(that: AbsValue)(using AbsState): AbsValue = ???

    /** comparison operations */
    def =^=(that: AbsValue)(using AbsState): AbsValue = ???
    def ==^==(that: AbsValue)(using AbsState): AbsValue = ???
    def <(that: AbsValue)(using AbsState): AbsValue = ???

    /** logical operations */
    def &&(that: AbsValue)(using AbsState): AbsValue = ???
    def ||(that: AbsValue)(using AbsState): AbsValue = ???
    def ^^(that: AbsValue)(using AbsState): AbsValue = ???

    /** numeric operations */
    def +(that: AbsValue)(using AbsState): AbsValue = ???
    def sub(that: AbsValue)(using AbsState): AbsValue = ???
    def /(that: AbsValue)(using AbsState): AbsValue = ???
    def *(that: AbsValue)(using AbsState): AbsValue = ???
    def %(that: AbsValue)(using AbsState): AbsValue = ???
    def %%(that: AbsValue)(using AbsState): AbsValue = ???
    def **(that: AbsValue)(using AbsState): AbsValue = ???
    def <<(that: AbsValue)(using AbsState): AbsValue = ???
    def >>(that: AbsValue)(using AbsState): AbsValue = ???
    def >>>(that: AbsValue)(using AbsState): AbsValue = ???

    /** unary negation operation */
    def unary_-(using AbsState): AbsValue = ???

    /** unary logical negation operation */
    def unary_!(using AbsState): AbsValue = ???

    /** unary bitwise negation operation */
    def unary_~(using AbsState): AbsValue = ???

    /** absolute operation */
    def abs(using AbsState): AbsValue = ???

    /** floor operation */
    def floor(using AbsState): AbsValue = ???

    /** type operations */
    def typeOf(using AbsState): AbsValue = ???

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = ???

  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = ???

    /** bottom element */
    lazy val Bot: AbsValue = ???

    /** useful abstract values */
    lazy val True = ???
    lazy val False = ???
    lazy val BoolTop = ???
    lazy val StrTop = ???
    lazy val NonNegInt = ???
    lazy val MathTop = ???
    lazy val NumberTop = ???
    lazy val BigIntTop = ???

    /** abstraction functions for an original value */
    def apply(value: Value): AbsValue = ???
    def apply(ast: Ast): AbsValue = ???
    def apply(n: Double): AbsValue = ???
    def apply(s: String): AbsValue = ???
    def apply(b: Boolean): AbsValue = ???
    def apply(d: BigDecimal): AbsValue = ???

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) => ???
  }
}
