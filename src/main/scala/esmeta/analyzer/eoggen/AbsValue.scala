package esmeta.analyzer.eoggen

import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Name, BOp, COp, VOp, MOp, UOp, Local, IRElem}
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: EOGGenerator =>
  import irStringifier.given
  import esStringifier.given
  import stateStringifier.given

  case class AbsValue(value: Flat[Value] = Zero) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean = value.isBottom

    /** single check */
    def getSingle(using st: AbsState): Flat[Value] = value

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      this.value <= that.value

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.value || that.value,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.value && that.value,
    )

    /** get lexical result */
    def getLexical(method: String): AbsValue = {
      val ret = value match
        case Zero | One(AstValue(_: Syntactic)) => Bot
        case One(AstValue(lex: Lexical)) =>
          val r = Interpreter.eval(lex, method)
          AbsValue(r)
        case _ => Top
      ret
    }

    /** get syntactic SDO */
    def getSdo(method: String): Flat[(AbsValue, Func)] = value match
      case Zero | One(AstValue(_: Lexical)) => Zero
      case One(AstValue(syn: Syntactic)) =>
        self.getSdo(syn, method).fold(Zero) { (thisValue, func) =>
          One(AbsValue(thisValue), func)
        }
      case _ => Many

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = this.toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = AbsValue(Many)

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** constructor for ASTs */
    def apply(ast: Ast): AbsValue = AbsValue(One(AstValue(ast)))

    /** constructor */
    def apply(value: Value): AbsValue = AbsValue(One(value))

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) => {
      given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
      given flatRule[T: Rule]: Rule[Flat[T]] = (app, flatElem) =>
        flatElem match
          case Zero   => app >> "⊥"
          case One(e) => app >> e
          case Many   => app >> "⊤"
      app >> elem.value
    }
  }
}
