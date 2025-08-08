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

  type AbsValueElem = Ast | Int | Boolean | String | Clo

  case class AbsValue(
    ast: Flat[Ast],
    int: Flat[Int],
    bool: Flat[Boolean],
    str: Flat[String],
    clo: Flat[Clo],
  ) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean =
      ast.isBottom &&
      int.isBottom &&
      bool.isBottom &&
      str.isBottom &&
      clo.isBottom

    /** single check */
    def getSingle(using st: AbsState): Flat[AbsValueElem] = this match
      case AbsValue(One(ast), Zero, Zero, Zero, Zero)  => One(ast)
      case AbsValue(Zero, One(int), Zero, Zero, Zero)  => One(int)
      case AbsValue(Zero, Zero, One(bool), Zero, Zero) => One(bool)
      case AbsValue(Zero, Zero, Zero, One(str), Zero)  => One(str)
      case AbsValue(Zero, Zero, Zero, Zero, One(clo))  => One(clo)
      case AbsValue(Zero, Zero, Zero, Zero, Zero)      => Zero
      case _                                           => Many

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      this.ast <= that.ast &&
      this.int <= that.int &&
      this.bool <= that.bool &&
      this.str <= that.str &&
      this.clo <= that.clo

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.ast || that.ast,
      this.int || that.int,
      this.bool || that.bool,
      this.str || that.str,
      this.clo || that.clo,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      this.ast && that.ast,
      this.int && that.int,
      this.bool && that.bool,
      this.str && that.str,
      this.clo && that.clo,
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
    lazy val Top: AbsValue = AbsValue(Many, Many, Many, Many, Many)

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue(Zero, Zero, Zero, Zero, Zero)

    /** AST top element */
    lazy val AstTop: AbsValue = AbsValue(Many, Zero, Zero, Zero, Zero)

    /** integer top element */
    lazy val IntTop: AbsValue = AbsValue(Zero, Many, Zero, Zero, Zero)

    /** boolean top element */
    lazy val BoolTop: AbsValue = AbsValue(Zero, Zero, Many, Zero, Zero)

    /** string top element */
    lazy val StrTop: AbsValue = AbsValue(Zero, Zero, Zero, Many, Zero)

    /** closure top element */
    lazy val CloTop: AbsValue = AbsValue(Zero, Zero, Zero, Zero, Many)

    /** non-AST top element */
    lazy val NonAst: AbsValue = AbsValue(Zero, Many, Many, Zero, Many)

    /** constructor for ASTs */
    def apply(ast: Ast): AbsValue = AbsValue(One(ast), Zero, Zero, Zero, Zero)

    /** constructor */
    def apply(value: Value): AbsValue = value match
      case AstValue(ast) => AbsValue(One(ast), Zero, Zero, Zero, Zero)
      case Math(d) if d.isWhole =>
        AbsValue(Zero, One(d.toInt), Zero, Zero, Zero)
      case Bool(b) => AbsValue(Zero, Zero, One(b), Zero, Zero)
      case Str(s)  => AbsValue(Zero, Zero, Zero, One(s), Zero)
      case Clo(func, captured) =>
        AbsValue(
          Zero,
          Zero,
          Zero,
          Zero,
          One(Clo(func, captured)),
        )
      case _ => Bot

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) => {
      given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
      given flatRule[T: Rule]: Rule[Flat[T]] = (app, flatElem) =>
        flatElem match
          case Zero   => app >> "⊥"
          case One(e) => app >> e
          case Many   => app >> "⊤"
      val AbsValue(ast, math, bool, str, clo) = elem
      app >> ast >> " | " >> math >> " | " >> bool >> " | " >> str >> " | " >> clo
    }
  }
}
