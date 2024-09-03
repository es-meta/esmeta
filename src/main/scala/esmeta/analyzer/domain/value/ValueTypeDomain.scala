package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.ir.{COp, Name, VOp, MOp, UOp, Local}
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
      ty: ValueTy,
      refinements: Refinements = Map(),
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract value")

    /** bottom element */
    val Bot: Elem = Elem(ValueTy())

    /** abstraction functions */
    def alpha(vs: Iterable[AValue]): Elem = Elem(getValueTy(vs))

    /** constructor with types */
    def apply(ty: Ty, refinements: Refinements): Elem = ty match
      case _: UnknownTy => Bot
      case vty: ValueTy => Elem(vty, refinements)

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
      import TyStringifier.given
      given Rule[Refinements] = sortedMapRule("{", "}", " => ")
      given Rule[Local] = (app, local) => app >> local.toString
      given Rule[Map[Local, ValueTy]] = sortedMapRule("[", "]", " <: ")
      given Rule[RefinementKind] = (app, kind) => app >> kind.toString
      given Ordering[RefinementKind] = Ordering.by(_.toString)
      given Ordering[Local] = Ordering.by(_.toString)
      app >> elem.ty
      if (elem.refinements.nonEmpty) app >> " " >> elem.refinements
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
        val keys = elem.refinements.keySet ++ that.refinements.keySet
        val refinements = keys.map { key =>
          val lmap = elem.refinements.getOrElse(key, Map.empty)
          val rmap = that.refinements.getOrElse(key, Map.empty)
          val keys = lmap.keySet ++ rmap.keySet
          key -> (keys.map { key =>
            val l = lmap.getOrElse(key, ValueTy.Bot)
            val r = rmap.getOrElse(key, ValueTy.Bot)
            key -> (l || r)
          }.toMap)
        }.toMap
        Elem(elem.ty || that.ty, refinements)

      /** meet operator */
      override def ⊓(that: Elem): Elem =
        val keys = elem.refinements.keySet intersect that.refinements.keySet
        val refinements = keys.map { key =>
          val lmap = elem.refinements.getOrElse(key, Map.empty)
          val rmap = that.refinements.getOrElse(key, Map.empty)
          val keys = lmap.keySet ++ rmap.keySet
          key -> (keys.map { key =>
            val l = lmap.getOrElse(key, ValueTy.Bot)
            val r = rmap.getOrElse(key, ValueTy.Bot)
            key -> (l ⊓ r)
          }.toMap)
        }.toMap
        Elem(elem.ty && that.ty, refinements)

      /** prune operator */
      override def --(that: Elem): Elem = Elem(elem.ty -- that.ty)

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
        Elem(ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt))

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
        Elem(ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt))

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
            BigIntT
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
      def getSdo(method: String): List[(Func, Elem)] = elem.ty.ast match
        case AstTy.Top =>
          for {
            func <- cfg.funcs if func.isSDO
            List(_, newMethod) <- allSdoPattern.unapplySeq(func.name)
            if newMethod == method
          } yield (func, Elem(AstT))
        case AstTy.Simple(names) =>
          for {
            name <- names.toList
            pair <- astSdoCache((name, method))
          } yield pair
        case AstTy.Detail(name, idx) =>
          synSdoCache((name, idx, method))

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
      def ty: ValueTy = elem.ty
      def refinements: Refinements = elem.refinements
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

    /** ast type check helper */
    lazy val astDirectChildMap: Map[String, Set[String]] =
      (cfg.grammar.prods.map {
        case Production(lhs, _, _, rhsList) =>
          val name = lhs.name
          val subs = rhsList.collect {
            case Rhs(_, List(Nonterminal(name, _)), _) => name
          }.toSet
          name -> subs
      }).toMap
    lazy val astChildMap: Map[String, Set[String]] =
      var descs = Map[String, Set[String]]()
      def aux(name: String): Set[String] = descs.get(name) match
        case Some(set) => set
        case None =>
          val set = (for {
            sub <- astDirectChildMap.getOrElse(name, Set())
            elem <- aux(sub)
          } yield elem) + name
          descs += name -> set
          set
      cfg.grammar.prods.foreach(prod => aux(prod.name))
      descs

    /** sdo access helper */
    private lazy val astSdoCache: ((String, String)) => List[(Func, Elem)] =
      cached[(String, String), List[(Func, Elem)]] {
        case (name, method) =>
          val result = (for {
            (fid, thisTy, hint) <- sdoMap.getOrElse(name, Set())
            if hint == method
          } yield (cfg.funcMap(fid), Elem(thisTy))).toList
          if (result.isEmpty) {
            if (defaultSdos contains method) {
              val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
              for {
                (rhs, idx) <- cfg.grammar.nameMap(name).rhsList.zipWithIndex
              } yield (defaultFunc, Elem(AstT(name, idx)))
            } else Nil
          } else result
      }
    private lazy val synSdoCache =
      cached[(String, Int, String), List[(Func, Elem)]] {
        case (name, idx, method) =>
          val result = (for {
            (fid, thisTy, hint) <- sdoMap.getOrElse(
              s"$name[$idx]",
              Set(),
            )
            if hint == method
          } yield (cfg.funcMap(fid), Elem(thisTy))).toList
          if (result.isEmpty) {
            if (defaultSdos contains method) {
              val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
              List((defaultFunc, Elem(AstT(name, idx))))
            } else Nil
          } else result
      }

    /** sdo with default case */
    val defaultSdos = List(
      "Contains",
      "AllPrivateIdentifiersValid",
      "ContainsArguments",
    )

    private lazy val allSdoPattern = """(<DEFAULT>|\w+\[\d+,\d+\])\.(\w+)""".r
    private lazy val sdoPattern = """(\w+)\[(\d+),(\d+)\]\.(\w+)""".r
    private lazy val sdoMap = {
      val edges: MMap[String, MSet[String]] = MMap()
      for {
        prod <- cfg.grammar.prods
        name = prod.name if !(cfg.grammar.lexicalNames contains name)
        (rhs, idx) <- prod.rhsList.zipWithIndex
        subIdx <- (0 until rhs.countSubs)
      } {
        val syntacticName = s"$name[$idx,$subIdx]"
        edges += (syntacticName -> MSet(name))
        rhs.getNts(subIdx) match
          case List(Some(chain)) =>
            if (edges contains chain) edges(chain) += syntacticName
            else edges(chain) = MSet(syntacticName)
          case _ =>
      }
      val worklist = QueueWorklist[String](List())
      val map: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
      var defaultmap: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
      for {
        func <- cfg.funcs if func.isSDO
        isDefaultSdo = func.name.startsWith("<DEFAULT>") if !isDefaultSdo
        List(name, idxStr, subIdxStr, method) <- sdoPattern.unapplySeq(
          func.name,
        )
        (idx, subIdx) = (idxStr.toInt, subIdxStr.toInt)
        key = s"$name[$idx,$subIdx]"
      } {
        val newInfo = (func.id, AstT(name, idx), method)
        val isDefaultSdo = defaultSdos contains method

        // update target info
        val targetmap = if (isDefaultSdo) defaultmap else map
        if (targetmap contains key) targetmap(key) += newInfo
        else targetmap(key) = MSet(newInfo)
        if (targetmap contains name) targetmap(name) += newInfo
        else targetmap(name) = MSet(newInfo)
      }

      // record original map
      val origmap = (for { (k, set) <- map } yield k -> set.toSet).toMap

      // propagate chain productions
      @tailrec
      def aux(): Unit = worklist.next match
        case Some(key) =>
          val childInfo = map.getOrElse(key, MSet())
          for {
            next <- edges.getOrElse(key, MSet())
            info = map.getOrElse(next, MSet())
            oldmapize = info.size

            newInfo =
              // A[i,j] -> A
              if (key endsWith "]") info ++ childInfo
              // A.method -> B[i,j].method
              // only if B[i,j].method not exists (chain production)
              else {
                val origInfo = origmap.getOrElse(next, Set())
                info ++ (for {
                  triple <- childInfo
                  if !(origInfo.exists(_._3 == triple._3))
                } yield triple)
              }

            _ = map(next) = newInfo
            if newInfo.size > oldmapize
          } worklist += next
          aux()
        case None => /* do nothing */
      aux()

      // merge default map
      (for {
        key <- map.keySet ++ defaultmap.keySet
        info = map.getOrElse(key, MSet())
        defaultInfo = defaultmap.getOrElse(key, MSet())
        finalInfo = (info ++ defaultInfo).toSet
      } yield key -> finalInfo).toMap
    }
  }
}
