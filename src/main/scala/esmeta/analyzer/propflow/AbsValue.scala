package esmeta.analyzer.propflow

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Name, BOp, COp, VOp, MOp, UOp, Local, IRElem}
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: PropFlowAnalyzer =>
  case class AbsValue(
    strings: Set[String] = Set(), // "toString" -> "toString"
    symbols: Set[String] = Set(), // %Symbol.toPrimitive% -> "toPrimitive"
  ) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean = strings.isEmpty && symbols.isEmpty

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      (this.strings subsetOf that.strings) &&
      (this.symbols subsetOf that.symbols)

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      strings = this.strings ++ that.strings,
      symbols = this.symbols ++ that.symbols,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue =
      AbsValue(
        strings = this.strings intersect that.strings,
        symbols = this.symbols intersect that.symbols,
      )

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = this.toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** create abstract value from strings */
    def string(s: String*): AbsValue = AbsValue(strings = s.toSet)

    /** create abstract value from symbol names */
    def symbol(s: String*): AbsValue = AbsValue(symbols = s.toSet)

    /** appender */
    given rule: Rule[AbsValue] = (app, value) => {
      given Rule[List[String]] = iterableRule("[", ", ", "]")
      val AbsValue(strings, symbols) = value
      val strs =
        strings.map("\"" + _ + "\"") ++
        symbols.map("@SYMBOL." + _)
      app >> strs.toList.sorted
    }
  }
}
