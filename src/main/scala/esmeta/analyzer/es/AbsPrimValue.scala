package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.domain.*

/** abstract primitive values */
trait AbsPrimValueDecl { self: ESAnalyzer =>

  // TODO more precise abstraction
  case class AbsPrimValue(set: BSet[PrimValue]) {

    /** bottom check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsPrimValue): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsPrimValue): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsPrimValue): AbsPrimValue = ???

    /** meet operator */
    def ⊓(that: AbsPrimValue): AbsPrimValue = ???
  }
  object AbsPrimValue extends Domain {
    type Elem = AbsPrimValue

    /** top element */
    lazy val Top: AbsPrimValue = ???

    /** bottom element */
    lazy val Bot: AbsPrimValue = ???

    /** useful abstract values */
    lazy val True = ???
    lazy val False = ???
    lazy val BoolTop = ???
    lazy val StrTop = ???
    lazy val NonNegInt = ???
    lazy val MathTop = ???
    lazy val NumberTop = ???
    lazy val BigIntTop = ???

    // abstraction functions
    def apply(vs: PrimValue*): AbsPrimValue = apply(vs)
    def apply(vs: Iterable[PrimValue]): AbsPrimValue = AbsPrimValue(BSet(vs))
    inline def apply(ast: Ast): AbsPrimValue = AbsPrimValue(AstValue(ast))
    inline def apply(n: Double): AbsPrimValue = AbsPrimValue(Number(n))
    inline def apply(s: String): AbsPrimValue = AbsPrimValue(Str(s))
    inline def apply(b: Boolean): AbsPrimValue = AbsPrimValue(Bool(b))
    inline def apply(d: BigDecimal): AbsPrimValue = AbsPrimValue(Math(d))
    inline def apply(i: scala.BigInt): AbsPrimValue = AbsPrimValue(BigInt(i))

    /** appender */
    given rule: Rule[AbsPrimValue] = (app, elem) => ???
  }
}
