package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract primitive values */
trait AbsPrimValueDecl { self: ESAnalyzer =>

  // TODO more precise abstraction
  type AbsPrimValue = AbsPrimValue.Elem
  object AbsPrimValue extends BSetDomain[PrimValue] {

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
    override def alpha(vs: Iterable[PrimValue]) = Fin(vs.toSet)
    inline def apply(ast: Ast): AbsPrimValue = alpha(AstValue(ast))
    inline def apply(n: Double): AbsPrimValue = alpha(Number(n))
    inline def apply(s: String): AbsPrimValue = alpha(Str(s))
    inline def apply(b: Boolean): AbsPrimValue = alpha(Bool(b))
    inline def apply(d: BigDecimal): AbsPrimValue = alpha(Math(d))
    inline def apply(i: scala.BigInt): AbsPrimValue = alpha(BigInt(i))

    import stateStringifier.given
    given rule: Rule[AbsPrimValue] = bsetRule
    given Ordering[PrimValue] = Ordering.by(_.toString)
  }
}
