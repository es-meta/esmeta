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

  case class AbsValue(
    ast: Flat[Ast],
    math: Flat[Math],
  ) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean = ast.isBottom && math.isBottom

    /** single check */
    def getSingle(using st: AbsState): Flat[Ast | Math] = this match
      case AbsValue(One(ast), Zero)  => One(ast)
      case AbsValue(Zero, One(math)) => One(math)
      case AbsValue(Zero, Zero)      => Zero
      case _                         => Many

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      this.ast <= that.ast && this.math <= that.math

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.ast || that.ast,
      this.math || that.math,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.ast && that.ast,
      this.math && that.math,
    )

    /** get lexical result */
    def getLexical(method: String): AbsValue = ast match
      case Zero | One(_: Syntactic) => Bot
      case One(lex: Lexical)        => AbsValue(Interpreter.eval(lex, method))
      case _                        => NonAst

    /** get syntactic SDO */
    def getSdo(method: String): Flat[(AbsValue, Func)] = ast match
      case Zero | One(_: Lexical) => Zero
      case One(syn: Syntactic) =>
        self.getSdo(syn, method).fold(Zero) { (thisValue, func) =>
          One(AbsValue(thisValue), func)
        }
      case _ => Many

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = this.toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = AbsValue(Many, Many)

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue(Zero, Zero)

    /** non-AST top element */
    lazy val NonAst: AbsValue = AbsValue(Zero, Many)

    /** constructor for ASTs */
    def apply(ast: Ast): AbsValue = AbsValue(One(ast), Zero)

    /** constructor */
    def apply(value: Value): AbsValue = value match
      case AstValue(ast) => AbsValue(One(ast), Zero)
      case math: Math    => AbsValue(Zero, One(math))
      case _             => Bot

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) => {
      given flatRule[T: Rule]: Rule[Flat[T]] = (app, flatElem) =>
        flatElem match
          case Zero   => app >> "⊥"
          case One(e) => app >> e
          case Many   => app >> "⊤"
      val AbsValue(ast, math) = elem
      app >> ast >> " | " >> math
    }
  }
}
