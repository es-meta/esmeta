package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{Name, BOp, COp, VOp, MOp, UOp, Local, IRElem}
import esmeta.interpreter.Interpreter
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.spec.{Grammar => _, *}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, Set => MSet}

trait ValueTypeDomainDecl { self: Self =>

  /** type domain for values */
  object ValueTypeDomain extends ValueDomain {

    /** elements */
    case class Elem(
      vty: ValueTy = BotT,
      expr: Option[SymExpr] = None,
      guard: TypeGuard = Map(),
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = Elem(AnyT, None, Map())

    /** bottom element */
    val Bot: Elem = Elem(BotT, None, Map())

    /** abstraction functions */
    def alpha(vs: Iterable[AValue]): Elem = Elem(getValueTy(vs))

    /** constructor with types */
    def apply(ty: Ty, guard: TypeGuard): Elem = ty match
      case _: UnknownTy => Bot
      case vty: ValueTy => Elem(vty, guard = guard)

    /** predefined top values */
    lazy val cloTop: Elem = Elem(CloT)
    lazy val contTop: Elem = Elem(ContT)
    lazy val partTop: Elem = notSupported("value.TypeDomain.partTop")
    lazy val astValueTop: Elem = Elem(AstT)
    lazy val grammarSymbolTop: Elem = notSupported(
      "value.TypeDomain.grammarSymbolTop",
    )
    lazy val codeUnitTop: Elem = Elem(CodeUnitT)
    lazy val enumTop: Elem = notSupported("value.TypeDomain.enumTop")
    lazy val mathTop: Elem = Elem(MathT)
    lazy val intTop: Elem = Elem(IntT)
    lazy val nonPosIntTop: Elem = Elem(NonPosIntT)
    lazy val nonNegIntTop: Elem = Elem(NonNegIntT)
    lazy val negIntTop: Elem = Elem(NegIntT)
    lazy val posIntTop: Elem = Elem(PosIntT)
    lazy val infinityTop: Elem = Elem(InfinityT)
    lazy val simpleValueTop: Elem =
      notSupported("value.TypeDomain.simpleValueTop")
    lazy val numberTop: Elem = Elem(NumberT)
    lazy val numberIntTop: Elem = Elem(NumberIntT)
    lazy val bigIntTop: Elem = Elem(BigIntT)
    lazy val strTop: Elem = Elem(StrT)
    lazy val boolTop: Elem = Elem(BoolT)
    lazy val undefTop: Elem = Elem(UndefT)
    lazy val nullTop: Elem = Elem(NullT)

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

    /** constructors */
    def apply(
      clo: AbsClo,
      cont: AbsCont,
      part: AbsPart,
      astValue: AbsAstValue,
      grammarSymbol: AbsGrammarSymbol,
      codeUnit: AbsCodeUnit,
      enumv: AbsEnum,
      math: AbsMath,
      infinity: AbsInfinity,
      simpleValue: AbsSimpleValue,
      num: AbsNumber,
      bigInt: AbsBigInt,
      str: AbsStr,
      bool: AbsBool,
      undef: AbsUndef,
      nullv: AbsNull,
    ): Elem = Top

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = None

    /** appender */
    given rule: Rule[Elem] = (app, elem) =>
      val irStringifier = IRElem.getStringifier(true, false)
      import TyStringifier.given
      import SymExpr.*, SymRef.*
      import irStringifier.given
      lazy val inlineField = "([_a-zA-Z][_a-zA-Z0-9]*)".r
      given Rule[SymExpr] = (app, expr) =>
        expr match
          case SEBool(bool) => app >> bool
          case SEStr(str)   => app >> "\"" >> normStr(str) >> "\""
          case SERef(ref)   => app >> ref
          case SETypeCheck(expr, ty) =>
            app >> "(? " >> expr >> ": " >> ty >> ")"
          case SEBinary(bop, left, right) =>
            app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
          case SEUnary(uop, expr) =>
            app >> "(" >> uop >> " " >> expr >> ")"
      given Rule[SymRef] = (app, ref) =>
        ref match
          case SSym(sym)                           => app >> "#" >> sym
          case SLocal(x)                           => app >> x
          case SField(base, SEStr(inlineField(f))) => app >> base >> "." >> f
          case SField(base, field) => app >> base >> "[" >> field >> "]"
      given Rule[TypeGuard] = sortedMapRule("{", "}", " => ")
      given Rule[Local] = (app, local) => app >> local.toString
      given Rule[Map[Local, ValueTy]] = sortedMapRule("[", "]", " <: ")
      given Rule[RefinementKind] = (app, kind) => app >> kind.toString
      given Ordering[RefinementKind] = Ordering.by(_.toString)
      given Ordering[Local] = Ordering.by(_.toString)
      app >> elem.vty
      elem.expr.map(app >> " | " >> _)
      if (elem.guard.nonEmpty) app >> " " >> elem.guard
      app

    /** transfer for variadic operation */
    def vopTransfer(vop: VOp, vs: List[Elem]): Elem = vop match
      case VOp.Min =>
        val math = vs.map(_.ty.math).reduce(_ min _)
        val inf = vs.map(_.ty.infinity).reduce(_ || _)
        Elem(
          ValueTy(
            math = math,
            infinity = if (math.isBottom) inf else inf && InfinityTy.Neg,
          ),
        )

      case VOp.Max =>
        val math = vs.map(_.ty.math).reduce(_ max _)
        val inf = vs.map(_.ty.infinity).reduce(_ || _)
        Elem(
          ValueTy(
            math = math,
            infinity = if (math.isBottom) inf else inf && InfinityTy.Pos,
          ),
        )
      case VOp.Concat => strTop

    /** transfer for mathematical operation */
    def mopTransfer(mop: MOp, vs: List[Elem]): Elem = mathTop

    /** element interfaces */
    extension (elem: Elem) {

      /** get key values */
      def keyValue: Elem = notSupported("value.TypeDomain.Elem.keyValue")

      /** partial order */
      def ⊑(that: Elem): Boolean = elem.ty <= that.ty

      /** join operator */
      def ⊔(that: Elem): Elem =
        import SymExpr.*
        val Elem(lty, lexpr, lguard) = elem
        val Elem(rty, rexpr, rguard) = that
        val kinds = lguard.keySet intersect rguard.keySet
        val guard = kinds.map { kind =>
          val g: Option[SymExpr] = lguard.get(kind)
          val expr = (lguard.get(kind), rguard.get(kind)) match
            case (Some(l), Some(r)) => SEBinary(BOp.Or, l, r)
            case _                  => SEBool(true)
          kind -> expr
        }.toMap
        (lexpr, rexpr) match
          case (_, None)           => Elem(lty || rty, lexpr, guard)
          case (None, _)           => Elem(lty || rty, rexpr, guard)
          case _ if lexpr == rexpr => Elem(lty || rty, lexpr, guard)
          case (Some(lexpr), Some(rexpr)) =>
            Elem(elem.ty || that.ty, None, guard)

      /** meet operator */
      override def ⊓(that: Elem): Elem =
        import SymExpr.*
        val Elem(lty, lexpr, lguard) = elem
        val Elem(rty, rexpr, rguard) = that
        val kinds = lguard.keySet ++ rguard.keySet
        val guard = kinds.map { kind =>
          val expr = (lguard.get(kind), rguard.get(kind)) match
            case (Some(l), Some(r)) => SEBinary(BOp.And, l, r)
            case (Some(l), None)    => l
            case (None, Some(r))    => r
            case _                  => SEBool(true)
          kind -> expr
        }.toMap
        (lexpr, rexpr) match
          case (Some(e), None) if getTy(e) <= rty =>
            Elem(lty && rty, lexpr, guard)
          case (None, Some(e)) if getTy(e) <= lty =>
            Elem(lty && rty, rexpr, guard)
          case _ if lexpr == rexpr => Elem(lty && rty, lexpr, guard)
          case _                   => Elem(elem.ty && that.ty, None, guard)

      /** prune operator */
      override def --(that: Elem): Elem =
        Elem(elem.ty -- that.ty, None, elem.guard)

      /** concretization function */
      override def gamma: BSet[AValue] = Inf

      /** get single string value */
      override def getSingle: Flat[AValue] = elem.ty.getSingle.map(AValue.from)

      /** get reachable address partitions */
      def reachableParts: Set[Part] = Set()

      /** bitwise operations */
      def &(that: Elem): Elem =
        mathOp(elem, that, "&") ⊔ bigIntOp(elem, that, "&")
      def |(that: Elem): Elem =
        mathOp(elem, that, "|") ⊔ bigIntOp(elem, that, "|")
      def ^(that: Elem): Elem =
        mathOp(elem, that, "^") ⊔ bigIntOp(elem, that, "^")

      /** comparison operations */
      def =^=(that: Elem): Elem =
        (elem.getSingle, that.getSingle) match
          case (Zero, _) | (_, Zero)       => Bot
          case (One(l), One(r))            => Elem(BoolT(l == r))
          case _ if (elem ⊓ that).isBottom => Elem(FalseT)
          case _                           => boolTop
      def ==^==(that: Elem): Elem = numericCompareOP(elem, that)
      def <(that: Elem): Elem = numericCompareOP(elem, that)

      /** logical operations */
      def &&(that: Elem): Elem = logicalOp(elem, that, "&&")
      def ||(that: Elem): Elem = logicalOp(elem, that, "||")
      def ^^(that: Elem): Elem = logicalOp(elem, that, "^")

      /** numeric operations */
      def +(that: Elem): Elem = numericOp(elem, that, "+")
      def sub(that: Elem): Elem = numericOp(elem, that, "-")
      def /(that: Elem): Elem = numericOp(elem, that, "/")
      def *(that: Elem): Elem = numericOp(elem, that, "*")
      def %(that: Elem): Elem = numericOp(elem, that, "%")
      def %%(that: Elem): Elem = numericOp(elem, that, "%%")
      def **(that: Elem): Elem = numericOp(elem, that, "**")
      def <<(that: Elem): Elem =
        mathOp(elem, that, "<<") ⊔ bigIntOp(elem, that, "<<")
      def >>(that: Elem): Elem =
        mathOp(elem, that, ">>") ⊔ bigIntOp(elem, that, ">>")
      def >>>(that: Elem): Elem = mathOp(elem, that, ">>>")

      /** unary negation operation */
      def unary_- : Elem =
        val mathTy = elem.ty.math match
          case MathTopTy      => MathTopTy
          case IntTy          => IntTy
          case NonPosIntTy    => NonNegIntTy
          case NonNegIntTy    => NonPosIntTy
          case PosIntTy       => NegIntTy
          case NegIntTy       => PosIntTy
          case MathSetTy(set) => MathSetTy(set.map(m => Math(-m.decimal)))
        val numberTy = elem.ty.number match
          case NumberTopTy      => NumberTopTy
          case NumberIntTy      => NumberIntTy
          case NumberSetTy(set) => NumberSetTy(set.map(n => Number(-n.double)))
        Elem(
          ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt),
        )

      /** unary logical negation operation */
      def unary_! : Elem = logicalUnaryOp(elem, "!")

      /** unary bitwise negation operation */
      def unary_~ : Elem =
        val mathTy = elem.ty.math match
          case MathTopTy | IntTy | NonPosIntTy => IntTy
          case NonNegIntTy | PosIntTy          => NegIntTy
          case NegIntTy                        => PosIntTy
          case MathSetTy(set) =>
            MathSetTy(set.map(m => Math(~(m.decimal.toInt))))
        val numberTy = elem.ty.number match
          case NumberTopTy      => NumberTopTy
          case NumberIntTy      => NumberIntTy
          case NumberSetTy(set) => NumberSetTy(set.filter(_.double.isWhole))
        Elem(
          ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt),
        )

      /** absolute operation */
      def abs: Elem =
        val mathTy = elem.ty.math match
          case MathTopTy                         => MathTopTy
          case IntTy | NonNegIntTy | NonPosIntTy => NonNegIntTy
          case NegIntTy | PosIntTy               => PosIntTy
          case MathSetTy(set) => MathSetTy(set.map(Interpreter.abs))
        Elem(ValueTy(math = mathTy))

      /** floor operation */
      def floor: Elem =
        val mathTy = elem.ty.math match
          case MathTopTy | IntTy                                     => IntTy
          case m @ (NonNegIntTy | NonPosIntTy | NegIntTy | PosIntTy) => m
          case MathSetTy(set) => MathSetTy(set.map(Interpreter.floor))
        Elem(ValueTy(math = mathTy))

      /** type operations */
      def typeOf(st: AbsState): Elem =
        val ty = elem.ty
        var names: Set[String] = Set()
        if (!ty.number.isBottom) names += "Number"
        if (ty.bigInt) names += "BigInt"
        if (!ty.str.isBottom) names += "String"
        if (!ty.bool.isBottom) names += "Boolean"
        if (ty.undef) names += "Undefined"
        if (ty.nullv) names += "Null"
        if (!(ty && ObjectT).isBottom) names += "Object"
        if (!(ty && SymbolT).isBottom) names += "Symbol"
        if (!(ty -- ESValueT).isBottom) names += "SpecType"
        Elem(StrT(names))

      /** type check */
      def typeCheck(ty: Ty, st: AbsState): Elem = boolTop

      /** helper functions for abstract transfer */
      def convertTo(cop: COp, radix: Elem): Elem =
        val ty = elem.ty
        Elem(cop match
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
      def sourceText: Elem = strTop
      def parse(rule: Elem): Elem = rule.ty.grammarSymbol match
        case Inf => exploded("too imprecise grammarSymbol rule for parsing")
        case Fin(set) =>
          Elem(AstT((for {
            grammarSymbol <- set
            name = grammarSymbol.name
          } yield name).toSet))
      def substring(from: Elem): Elem = strTop
      def substring(from: Elem, to: Elem): Elem = strTop
      def trim(isStarting: Boolean): Elem = strTop
      def instanceOf(ty: Elem): Elem = boolTop
      def sizeOf(st: AbsState): Elem = nonNegIntTop
      def clamp(lower: Elem, upper: Elem): Elem =
        val xty = elem.ty.math
        val lowerTy = lower.ty.math
        val upperTy = upper.ty.math
        val mathTy =
          if (xty.isInt)
            if (lowerTy.isPosInt) PosIntTy
            else if (lowerTy.isNonNegInt) NonNegIntTy
            else if (upperTy.isNegInt) NegIntTy
            else if (upperTy.isNonPosInt) NonPosIntTy
            else IntTy
          else MathTopTy
        Elem(ValueTy(math = mathTy))

      /** refine receiver object */
      def refineThis(func: Func): Elem = elem

      /** get lexical result */
      def getLexical(method: String): Elem = Elem(
        if (elem.ty.ast.isBottom) ValueTy()
        else
          method match
            case "SV" | "TRV" | "StringValue" => StrT
            // TODO handle `list of code points` type
            case "IdentifierCodePoints" => StrT
            case "MV" =>
              elem.ty.ast.names match
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

      /** get syntactic SDO */
      def getSdo(method: String): List[(Func, Elem)] =
        import cfg.sdoInfo.*
        elem.ty.ast match
          case AstTy.Top =>
            for {
              base <- noBase.getOrElse(method, Set()).toList
            } yield base.func -> Elem(base.thisTy)
          case AstTy.Simple(names) =>
            for {
              name <- names.toList
              base <- simple.getOrElse((name, method), Set())
            } yield base.func -> Elem(base.thisTy)
          case AstTy.Detail(name, idx) =>
            for {
              base <- indexed.getOrElse(((name, idx), method), Set()).toList
            } yield base.func -> Elem(base.thisTy)

      /** getters */
      def clo: AbsClo = ty.clo match
        case Inf => AbsClo.Top
        case Fin(set) =>
          AbsClo(for {
            name <- set.toList
            if cfg.fnameMap.contains(name)
          } yield AClo(cfg.fnameMap(name), Map())) // TODO captured
      def cont: AbsCont = ty.cont match
        case Inf => AbsCont.Top
        case Fin(set) =>
          AbsCont(for {
            fid <- set.toList
            node = cfg.nodeMap(fid)
            func = cfg.funcOf(node)
          } yield ACont(NodePoint(func, node, View()), Map())) // TODO captured
      def part: AbsPart = notSupported("ValueTypeDomain.Elem.part")
      def astValue: AbsAstValue = notSupported("ValueTypeDomain.Elem.ast")
      def grammarSymbol: AbsGrammarSymbol = notSupported(
        "ValueTypeDomain.Elem.grammarSymbol",
      )
      def codeUnit: AbsCodeUnit = notSupported("ValueTypeDomain.Elem.codeUnit")
      def enumv: AbsEnum = notSupported("ValueTypeDomain.Elem.enumv")
      def math: AbsMath = notSupported("ValueTypeDomain.Elem.math")
      def infinity: AbsInfinity = notSupported("ValueTypeDomain.Elem.infinity")
      def simpleValue: AbsSimpleValue =
        notSupported("ValueTypeDomain.Elem.simpleValue")
      def number: AbsNumber = notSupported("ValueTypeDomain.Elem.number")
      def bigInt: AbsBigInt = notSupported("ValueTypeDomain.Elem.bigInt")
      def str: AbsStr = notSupported("ValueTypeDomain.Elem.str")
      def bool: AbsBool = AbsBool.alpha(elem.ty.bool.set.map(Bool.apply))
      def undef: AbsUndef = notSupported("ValueTypeDomain.Elem.undef")
      def nullv: AbsNull = notSupported("ValueTypeDomain.Elem.nullv")
      def ty: ValueTy = elem.vty // TODO elem.expr
      def expr: Option[SymExpr] = elem.expr
      def guard: TypeGuard = elem.guard
    }

    // -------------------------------------------------------------------------
    // private helpers
    // -------------------------------------------------------------------------
    // value type getter
    private def getValueTy(vs: Iterable[AValue]): ValueTy =
      vs.foldLeft(ValueTy()) { case (vty, v) => vty || getValueTy(v) }

    // value type getter
    private def getValueTy(v: AValue): ValueTy = v match
      case AClo(func, _)                => CloT(func.name)
      case ACont(target, _)             => ContT(target.node.id)
      case AstValue(ast)                => AstT(ast.name)
      case grammarSymbol: GrammarSymbol => GrammarSymbolT(grammarSymbol)
      case CodeUnit(_)                  => CodeUnitT
      case Enum(name)                   => EnumT(name)
      case Math(n)                      => MathT(n)
      case Infinity(pos)                => InfinityT(pos)
      case n: Number                    => NumberT(n)
      case BigInt(_)                    => BigIntT
      case Str(n)                       => StrT(n)
      case Bool(true)                   => TrueT
      case Bool(false)                  => FalseT
      case Undef                        => UndefT
      case Null                         => NullT
      case v => notSupported(s"impossible to convert to pure type ($v)")

    /** type getter for symbolic expressions */
    private def getTy(sexpr: SymExpr): ValueTy = ???

    // numeric operator helper
    private def numericOp(l: Elem, r: Elem, op: String) =
      mathOp(l, r, op) ⊔ numberOp(l, r, op) ⊔ bigIntOp(l, r, op)

    // mathematical operator helper
    private def mathOp(l: Elem, r: Elem, op: String) =
      val lty = l.ty.math
      val rty = r.ty.math
      op match
        case _ if lty.isBottom || rty.isBottom => Bot
        case "+"   => Elem(ValueTy(math = lty + rty))
        case "-"   => Elem(ValueTy(math = lty - rty))
        case "%"   => Elem(ValueTy(math = lty % rty))
        case "**"  => Elem(ValueTy(math = lty ** rty))
        case "*"   => Elem(ValueTy(math = lty * rty))
        case "&"   => Elem(ValueTy(math = lty & rty))
        case "|"   => Elem(ValueTy(math = lty | rty))
        case "^"   => Elem(ValueTy(math = lty ^ rty))
        case "<<"  => Elem(ValueTy(math = lty << rty))
        case ">>"  => Elem(ValueTy(math = lty >> rty))
        case ">>>" => Elem(ValueTy(math = lty >>> rty))
        case _     => mathTop

    // number operator helper
    private def numberOp(l: Elem, r: Elem, op: String) =
      val lty = l.ty.number
      val rty = r.ty.number
      if (lty.isBottom || rty.isBottom) Bot
      else numberTop

    // big integer operator helper
    private def bigIntOp(l: Elem, r: Elem, op: String) =
      val lty = l.ty.bigInt
      val rty = r.ty.bigInt
      if (!lty || !rty) Bot
      else bigIntTop

    // logical unary operator helper
    private def logicalUnaryOp(b: Elem, op: "!") =
      Elem(BoolT(for {
        x <- b.ty.bool.set
      } yield op match
        case "!" => !x,
      ))

    // logical operator helper
    private def logicalOp(l: Elem, r: Elem, op: "&&" | "||" | "^") =
      Elem(BoolT(for {
        x <- l.ty.bool.set
        y <- r.ty.bool.set
      } yield op match
        case "&&" => x && y
        case "||" => x || y
        case "^"  => x ^ y,
      ))

    // numeric comparison operator helper
    private lazy val numericCompareOP: (Elem, Elem) => Elem = (l, r) =>
      Elem(
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
  }
}
