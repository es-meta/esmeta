package esmeta.analyzer.tychecker

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Name, BOp, COp, VOp, MOp, UOp, Local, IRElem}
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: TyChecker =>

  case class AbsValue(
    lowerTy: ValueTy = BotT,
    expr: Flat[SymExpr] = Zero,
    guard: TypeGuard = TypeGuard.Empty,
  ) extends AbsValueLike {
    import AbsValue.*

    /** bottom check */
    def isBottom: Boolean = lowerTy.isBottom && (expr == Zero || expr == Many)

    /** upper type */
    def ty(using st: AbsState): ValueTy = expr match
      case Zero      => lowerTy
      case One(expr) => lowerTy || st.getTy(expr)
      case Many      => lowerTy

    /** single check */
    def isSingle: Boolean =
      expr == Zero && guard.isEmpty && (lowerTy.getSingle match
        case One(_) => true
        case _      => false
      )

    /** partial order */
    def ⊑(that: AbsValue)(using AbsState): Boolean =
      val l @ AbsValue(llty, lexpr, lguard) = this
      val r @ AbsValue(rlty, rexpr, rguard) = that
      (lguard <= rguard) && ((lexpr, rexpr) match
        case (Zero, Zero)     => llty ⊑ rlty
        case (Zero, One(_))   => llty ⊑ rlty
        case (One(x), One(y)) => (llty ⊑ rlty && x == y) || (l.ty ⊑ r.ty)
        case (One(_), Many)   => l.ty ⊑ rlty
        case (Zero, Many)     => llty ⊑ rlty
        case (Many, Many)     => llty ⊑ rlty
        case _                => false
      )

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using AbsState): AbsValue =
      import SymExpr.*
      val l @ AbsValue(llty, lexpr, lguard) = this
      val r @ AbsValue(rlty, rexpr, rguard) = that
      val guard = lguard || rguard
      (lexpr, rexpr) match
        case (Zero, Zero)               => AbsValue(llty || rlty, Zero, guard)
        case (Zero, One(r))             => AbsValue(llty || rlty, One(r), guard)
        case (One(l), Zero)             => AbsValue(llty || rlty, One(l), guard)
        case (One(l), One(r)) if l == r => AbsValue(llty || rlty, One(l), guard)
        case (One(_), One(_))           => AbsValue(l.ty || r.ty, Many, guard)
        case (One(_), Many)             => AbsValue(l.ty || rlty, Many, guard)
        case (Many, One(_))             => AbsValue(llty || r.ty, Many, guard)
        case (Zero, Many)               => AbsValue(llty || rlty, Many, guard)
        case (Many, Zero)               => AbsValue(llty || rlty, Many, guard)
        case (Many, Many)               => AbsValue(llty || rlty, Many, guard)

    /** meet operator */
    def ⊓(that: AbsValue)(using AbsState): AbsValue =
      import SymExpr.*
      val l @ AbsValue(llty, lexpr, lguard) = this
      val r @ AbsValue(rlty, rexpr, rguard) = that
      val guard = lguard && rguard
      (lexpr, rexpr) match
        case (Zero, Zero)               => AbsValue(llty && rlty, Zero, guard)
        case (Zero, One(_))             => AbsValue(llty && r.ty, Zero, guard)
        case (One(_), Zero)             => AbsValue(l.ty && rlty, Zero, guard)
        case (One(l), One(r)) if l == r => AbsValue(llty && rlty, One(l), guard)
        case (One(_), One(_))           => AbsValue(l.ty && r.ty, Many, guard)
        case (One(_), Many)             => AbsValue(l.ty && rlty, Many, guard)
        case (Many, One(_))             => AbsValue(llty && r.ty, Many, guard)
        case (Zero, Many)               => AbsValue(llty && rlty, Many, guard)
        case (Many, Zero)               => AbsValue(llty && rlty, Many, guard)
        case (Many, Many)               => AbsValue(llty && rlty, Many, guard)

    /** prune operator */
    def --(that: AbsValue)(using AbsState): AbsValue =
      this.copy(lowerTy = this.lowerTy -- that.lowerTy)

    /** add type guard */
    def addGuard(guard: TypeGuard): AbsValue =
      this.copy(guard = this.guard && guard)

    /** has symbols */
    def has(sym: Sym): Boolean = expr match
      case One(expr) => expr.has(sym)
      case _         => false

    /** get symbolic expression when it only has a symbolic expression */
    def getSymExpr: Option[SymExpr] = expr match
      case One(expr) if lowerTy.isBottom => Some(expr)
      case _                             => None

    /** get lexical result */
    def getLexical(method: String)(using AbsState): AbsValue = {
      val ty = this.ty
      if (ty.ast.isBottom) Bot
      else
        AbsValue(method match
          case "SV" | "TRV" | "StringValue" => StrT
          // TODO handle `list of code points` type
          case "IdentifierCodePoints" => StrT
          case "MV" =>
            ty.ast.names match
              case Fin(set) =>
                if (set subsetOf posIntMVTyNames) PosIntT
                else if (set subsetOf nonNegIntMVTyNames) NonNegIntT
                else MathT
              case Inf => MathT
          case "NumericValue"          => NumericT
          case "TV"                    => StrT // XXX ignore UndefT case
          case "BodyText" | "FlagText" => StrT
          case "Contains"              => BoolT
          case _                       => ValueTy(),
        )
    }

    /** get syntactic SDO */
    def getSdo(method: String)(using AbsState): List[(Func, AbsValue)] = {
      import cfg.sdoInfo.*
      this.ty.ast match
        case AstTy.Top =>
          for {
            base <- noBase.getOrElse(method, Set()).toList
          } yield base.func -> AbsValue(base.thisTy)
        case AstTy.Simple(names) =>
          for {
            name <- names.toList
            base <- simple.getOrElse((name, method), Set())
          } yield base.func -> AbsValue(base.thisTy)
        case AstTy.Detail(name, idx) =>
          for {
            base <- indexed.getOrElse(((name, idx), method), Set()).toList
          } yield base.func -> AbsValue(base.thisTy)
    }

    /** parse strings with a rule */
    def parse(rule: AbsValue)(using AbsState): AbsValue = {
      rule.ty.grammarSymbol match
        case Inf => exploded("too imprecise grammarSymbol rule for parsing")
        case Fin(set) =>
          AbsValue(AstT((for {
            grammarSymbol <- set
            name = grammarSymbol.name
          } yield name).toSet))
    }

    def substring(from: AbsValue): AbsValue = StrTop
    def substring(from: AbsValue, to: AbsValue): AbsValue = StrTop
    def trim(isStarting: Boolean): AbsValue = StrTop
    def instanceOf(ty: AbsValue): AbsValue = BoolTop
    def sizeOf: AbsValue = NonNegInt

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: AbsValue)(using AbsState): AbsValue = {
      val ty = this.ty
      AbsValue(cop match
        case COp.ToApproxNumber =>
          if (!ty.math.isBottom) NumberT
          else ValueTy.Bot
        case COp.ToNumber =>
          lazy val fromMath = ty.math match
            case m if m.isInt => NumberIntTy
            case _            => NumberTopTy
          if (!ty.str.isBottom) NumberT
          else ValueTy(number = ty.number || fromMath)
        case COp.ToBigInt
            if (!ty.math.isBottom || !ty.str.isBottom || !ty.number.isBottom || ty.bigInt) =>
          if (!ty.str.isBottom) BigIntT || UndefT
          else BigIntT
        case COp.ToMath =>
          val fromNumber = ty.number match
            case NumberTopTy      => MathTopTy
            case NumberIntTy      => IntTy
            case NumberSetTy(set) => MathSetTy(set.map(n => Math(n.double)))
          val fromBigInt = if (ty.bigInt) IntTy else MathTy.Bot
          ValueTy(math = ty.math || fromNumber || fromBigInt)
        case COp.ToStr(_)
            if (!ty.str.isBottom || !ty.number.isBottom || ty.bigInt) =>
          StrT
        case _ => ValueTy(),
      )
    }

    /** bitwise operations */
    def &(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, "&") ⊔ bigIntOp(this, that, "&")
    def |(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, "|") ⊔ bigIntOp(this, that, "|")
    def ^(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, "^") ⊔ bigIntOp(this, that, "^")

    /** comparison operations */
    def =^=(that: AbsValue)(using AbsState): AbsValue =
      if (this.isBottom || that.isBottom) Bot
      else if ((this ⊓ that).isBottom) False
      else if (this == that && this.isSingle && that.isSingle) True
      else BoolTop
    def ==^==(that: AbsValue)(using AbsState): AbsValue =
      numericCompareOP(this, that)
    def <(that: AbsValue)(using AbsState): AbsValue =
      numericCompareOP(this, that)

    /** logical operations */
    def &&(that: AbsValue)(using AbsState): AbsValue =
      logicalOp(this, that, "&&")
    def ||(that: AbsValue)(using AbsState): AbsValue =
      logicalOp(this, that, "||")
    def ^^(that: AbsValue)(using AbsState): AbsValue =
      logicalOp(this, that, "^")

    /** numeric operations */
    def +(that: AbsValue)(using AbsState): AbsValue = numericOp(this, that, "+")
    def sub(that: AbsValue)(using AbsState): AbsValue =
      numericOp(this, that, "-")
    def /(that: AbsValue)(using AbsState): AbsValue = numericOp(this, that, "/")
    def *(that: AbsValue)(using AbsState): AbsValue = numericOp(this, that, "*")
    def %(that: AbsValue)(using AbsState): AbsValue = numericOp(this, that, "%")
    def %%(that: AbsValue)(using AbsState): AbsValue =
      numericOp(this, that, "%%")
    def **(that: AbsValue)(using AbsState): AbsValue =
      numericOp(this, that, "**")
    def <<(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, "<<") ⊔ bigIntOp(this, that, "<<")
    def >>(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, ">>") ⊔ bigIntOp(this, that, ">>")
    def >>>(that: AbsValue)(using AbsState): AbsValue =
      mathOp(this, that, ">>>")

    /** unary negation operation */
    def unary_-(using AbsState): AbsValue =
      val ty = this.ty
      val mathTy = ty.math match
        case MathTopTy      => MathTopTy
        case IntTy          => IntTy
        case NonPosIntTy    => NonNegIntTy
        case NonNegIntTy    => NonPosIntTy
        case PosIntTy       => NegIntTy
        case NegIntTy       => PosIntTy
        case MathSetTy(set) => MathSetTy(set.map(m => Math(-m.decimal)))
      val numberTy = ty.number match
        case NumberTopTy      => NumberTopTy
        case NumberIntTy      => NumberIntTy
        case NumberSetTy(set) => NumberSetTy(set.map(n => Number(-n.double)))
      AbsValue(
        ValueTy(math = mathTy, number = numberTy, bigInt = this.ty.bigInt),
      )

    /** unary logical negation operation */
    def unary_!(using AbsState): AbsValue = logicalUnaryOp(this, "!")

    /** unary bitwise negation operation */
    def unary_~(using AbsState): AbsValue =
      val ty = this.ty
      val mathTy = ty.math match
        case MathTopTy | IntTy | NonPosIntTy => IntTy
        case NonNegIntTy | PosIntTy          => NegIntTy
        case NegIntTy                        => PosIntTy
        case MathSetTy(set) =>
          MathSetTy(set.map(m => Math(~(m.decimal.toInt))))
      val numberTy = ty.number match
        case NumberTopTy      => NumberTopTy
        case NumberIntTy      => NumberIntTy
        case NumberSetTy(set) => NumberSetTy(set.filter(_.double.isWhole))
      AbsValue(
        ValueTy(math = mathTy, number = numberTy, bigInt = ty.bigInt),
      )

    /** absolute operation */
    def abs(using AbsState): AbsValue =
      val mathTy = this.ty.math match
        case MathTopTy                         => MathTopTy
        case IntTy | NonNegIntTy | NonPosIntTy => NonNegIntTy
        case NegIntTy | PosIntTy               => PosIntTy
        case MathSetTy(set) => MathSetTy(set.map(Interpreter.abs))
      AbsValue(ValueTy(math = mathTy))

    /** floor operation */
    def floor(using AbsState): AbsValue =
      val mathTy = this.ty.math match
        case MathTopTy | IntTy                                     => IntTy
        case m @ (NonNegIntTy | NonPosIntTy | NegIntTy | PosIntTy) => m
        case MathSetTy(set) => MathSetTy(set.map(Interpreter.floor))
      AbsValue(ValueTy(math = mathTy))

    /** type operations */
    def typeOf(using AbsState): AbsValue = AbsValue(StrT(this.ty.typeOfNames))

    // numeric operator helper
    private def numericOp(
      l: AbsValue,
      r: AbsValue,
      op: String,
    )(using AbsState) =
      mathOp(l, r, op) ⊔ numberOp(l, r, op) ⊔ bigIntOp(l, r, op)

    // mathematical operator helper
    private def mathOp(
      l: AbsValue,
      r: AbsValue,
      op: String,
    )(using AbsState) =
      val lty = l.ty.math
      val rty = r.ty.math
      op match
        case _ if lty.isBottom || rty.isBottom => Bot
        case "+"   => AbsValue(ValueTy(math = lty + rty))
        case "-"   => AbsValue(ValueTy(math = lty - rty))
        case "%"   => AbsValue(ValueTy(math = lty % rty))
        case "**"  => AbsValue(ValueTy(math = lty ** rty))
        case "*"   => AbsValue(ValueTy(math = lty * rty))
        case "&"   => AbsValue(ValueTy(math = lty & rty))
        case "|"   => AbsValue(ValueTy(math = lty | rty))
        case "^"   => AbsValue(ValueTy(math = lty ^ rty))
        case "<<"  => AbsValue(ValueTy(math = lty << rty))
        case ">>"  => AbsValue(ValueTy(math = lty >> rty))
        case ">>>" => AbsValue(ValueTy(math = lty >>> rty))
        case _     => MathTop

    // number operator helper
    private def numberOp(
      l: AbsValue,
      r: AbsValue,
      op: String,
    )(using AbsState) =
      val lty = l.ty.number
      val rty = r.ty.number
      if (lty.isBottom || rty.isBottom) Bot
      else NumberTop

    // big integer operator helper
    private def bigIntOp(
      l: AbsValue,
      r: AbsValue,
      op: String,
    )(using AbsState) =
      val lty = l.ty.bigInt
      val rty = r.ty.bigInt
      if (!lty || !rty) Bot
      else BigIntTop

    // logical unary operator helper
    private def logicalUnaryOp(
      b: AbsValue,
      op: "!",
    )(using AbsState) = AbsValue(BoolT(for {
      x <- b.ty.bool.set
    } yield op match
      case "!" => !x,
    ))

    // logical operator helper
    private def logicalOp(
      l: AbsValue,
      r: AbsValue,
      op: "&&" | "||" | "^",
    )(using AbsState) =
      AbsValue(BoolT(for {
        x <- l.ty.bool.set
        y <- r.ty.bool.set
      } yield op match
        case "&&" => x && y
        case "||" => x || y
        case "^"  => x ^ y,
      ))

    // numeric comparison operator helper
    private def numericCompareOP(
      l: AbsValue,
      r: AbsValue,
    )(using AbsState): AbsValue = AbsValue(
      ValueTy(
        bool = BoolTy(
          if (
            (
              (!l.ty.math.isBottom || !l.ty.number.isBottom) &&
              (!r.ty.math.isBottom || !r.ty.number.isBottom)
            ) || (l.ty.bigInt && r.ty.bigInt)
          ) Set(true, false)
          else Set(),
        ),
      ),
    )

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String =
      given AbsState = state
      import TyStringifier.given
      s"$this (${ty})"

  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = AbsValue(AnyT, Many, TypeGuard.Empty)

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue(BotT, Zero, TypeGuard.Empty)

    /** useful abstract values */
    lazy val True = AbsValue(TrueT)
    lazy val False = AbsValue(FalseT)
    lazy val BoolTop = AbsValue(BoolT)
    lazy val StrTop = AbsValue(StrT)
    lazy val NonNegInt = AbsValue(NonNegIntT)
    lazy val MathTop = AbsValue(MathT)
    lazy val NumberTop = AbsValue(NumberT)
    lazy val BigIntTop = AbsValue(BigIntT)

    /** TODO AST type names whose MV returns a positive integer */
    lazy val posIntMVTyNames: Set[String] = Set(
      "NonZeroDigit",
    )

    /** TODO AST type names whose MV returns a non-negative integer */
    lazy val nonNegIntMVTyNames: Set[String] = Set(
      "CodePoint",
      "Hex4Digits",
      "HexEscapeSequence",
    ) ++ posIntMVTyNames

    /** appender */
    given rule: Rule[AbsValue] = (app, elem) =>
      val irStringifier = IRElem.getStringifier(true, false)
      import TyStringifier.given
      import irStringifier.given
      given Rule[Map[Local, ValueTy]] = sortedMapRule("[", "]", " <: ")
      given Ordering[Local] = Ordering.by(_.toString)
      val AbsValue(lowerTy, expr, guard) = elem
      if (!lowerTy.isBottom || expr == Zero)
        app >> lowerTy
        if (expr != Zero) app >> " | "
      expr match
        case Zero      =>
        case One(expr) => app >> expr
        case Many      => app >> "*"
      if (guard.nonEmpty) app >> " " >> guard
      app
  }
}
