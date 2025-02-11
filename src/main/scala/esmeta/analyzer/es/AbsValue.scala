package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.error.*
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: ESAnalyzer =>

  case class AbsValue(
    addr: AbsAddr = AbsAddr.Bot,
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    prim: AbsPrimValue = AbsPrimValue.Bot,
  ) {
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

  }
  object AbsValue extends ValueDomain {

    /** top element */
    lazy val Top: AbsValue = ???

    /** bottom element */
    lazy val Bot: AbsValue = ???

    /** useful abstract values */
    lazy val True = AbsValue(prim = AbsPrimValue.True)
    lazy val False = AbsValue(prim = AbsPrimValue.False)
    lazy val BoolTop = AbsValue(prim = AbsPrimValue.BoolTop)
    lazy val StrTop = AbsValue(prim = AbsPrimValue.StrTop)
    lazy val NonNegInt = AbsValue(prim = AbsPrimValue.NonNegInt)
    lazy val MathTop = AbsValue(prim = AbsPrimValue.MathTop)
    lazy val NumberTop = AbsValue(prim = AbsPrimValue.NumberTop)
    lazy val BigIntTop = AbsValue(prim = AbsPrimValue.BigIntTop)

    // abstraction functions
    def apply(vs: Value*): AbsValue = apply(vs)
    def apply(vs: Iterable[Value]): AbsValue = {
      var addrs = Set[Addr]()
      var prims = Set[PrimValue]()
      vs.map {
        case prim: PrimValue => prims += prim
        case addr: NamedAddr => addrs += addr
        case v               => throw InvalidAbstraction(v)
      }
      AbsValue(addr = AbsAddr(addrs), prim = AbsPrimValue(prims))
    }
    inline def apply(ast: Ast): AbsValue = AbsValue(prim = AbsPrimValue(ast))
    inline def apply(n: Double): AbsValue = AbsValue(prim = AbsPrimValue(n))
    inline def apply(s: String): AbsValue = AbsValue(prim = AbsPrimValue(s))
    inline def apply(b: Boolean): AbsValue = AbsValue(prim = AbsPrimValue(b))
    inline def apply(d: BigDecimal): AbsValue = AbsValue(prim = AbsPrimValue(d))

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) => ???

    extension (value: AbsValue) {
      def getString(state: AbsState): String = ???
    }
  }
}
