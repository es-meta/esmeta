package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.ir.{COp, Name, VOp, MOp, Local}
import esmeta.parser.ESValueParser
import esmeta.util.*
import esmeta.util.Appender.*

trait ValueBasicDomainDecl { self: Self =>

  /** basic domain for values */
  object ValueBasicDomain extends ValueDomain {

    /** elements */
    case class Elem(
      clo: AbsClo = AbsClo.Bot,
      cont: AbsCont = AbsCont.Bot,
      part: AbsPart = AbsPart.Bot,
      astValue: AbsAstValue = AbsAstValue.Bot,
      grammarSymbol: AbsGrammarSymbol = AbsGrammarSymbol.Bot,
      codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
      enumv: AbsEnum = AbsEnum.Bot,
      math: AbsMath = AbsMath.Bot,
      infinity: AbsInfinity = AbsInfinity.Bot,
      simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract value")

    /** bottom element */
    val Bot: Elem = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[AValue]): Elem = Elem(
      AbsClo(xs.collect { case x: AClo => x }),
      AbsCont(xs.collect { case x: ACont => x }),
      AbsPart(xs.collect { case x: Part => x }),
      AbsAstValue(xs.collect { case x: AstValue => x }),
      AbsGrammarSymbol(xs.collect { case x: GrammarSymbol => x }),
      AbsCodeUnit(xs.collect { case x: CodeUnit => x }),
      AbsEnum(xs.collect { case x: Enum => x }),
      AbsMath(xs.collect { case x: Math => x }),
      AbsInfinity(xs.collect { case x: Infinity => x }),
      AbsSimpleValue(xs.collect { case x: SimpleValue => x }),
    )

    /** constructor with types */
    def apply(ty: Ty, refinements: Refinements) = Top

    /** predefined top values */
    val cloTop: Elem = Bot.copy(clo = AbsClo.Top)
    val contTop: Elem = Bot.copy(cont = AbsCont.Top)
    val partTop: Elem = Bot.copy(part = AbsPart.Top)
    val astValueTop: Elem = Bot.copy(astValue = AbsAstValue.Top)
    val grammarSymbolTop: Elem = Bot.copy(grammarSymbol = AbsGrammarSymbol.Top)
    val codeUnitTop: Elem = Bot.copy(codeUnit = AbsCodeUnit.Top)
    val enumTop: Elem = Bot.copy(enumv = AbsEnum.Top)
    val mathTop: Elem = Bot.copy(math = AbsMath.Top)
    val infinityTop: Elem = Bot.copy(infinity = AbsInfinity.Top)
    val simpleValueTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.Top)
    val numberTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.numberTop)
    val bigIntTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.bigIntTop)
    val strTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.strTop)
    val boolTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.boolTop)
    val undefTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.undefTop)
    val nullTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.nullTop)

    /** constructors */
    def apply(
      clo: AbsClo = AbsClo.Bot,
      cont: AbsCont = AbsCont.Bot,
      part: AbsPart = AbsPart.Bot,
      astValue: AbsAstValue = AbsAstValue.Bot,
      grammarSymbol: AbsGrammarSymbol = AbsGrammarSymbol.Bot,
      codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
      enumv: AbsEnum = AbsEnum.Bot,
      math: AbsMath = AbsMath.Bot,
      infinity: AbsInfinity = AbsInfinity.Bot,
      simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
      num: AbsNumber = AbsNumber.Bot,
      bigInt: AbsBigInt = AbsBigInt.Bot,
      str: AbsStr = AbsStr.Bot,
      bool: AbsBool = AbsBool.Bot,
      undef: AbsUndef = AbsUndef.Bot,
      nullv: AbsNull = AbsNull.Bot,
    ): Elem = Elem(
      clo,
      cont,
      part,
      astValue,
      grammarSymbol,
      codeUnit,
      enumv,
      math,
      infinity,
      simpleValue ⊔ AbsSimpleValue(num, bigInt, str, bool, undef, nullv),
    )

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = Some(
      (
        elem.clo,
        elem.cont,
        elem.part,
        elem.astValue,
        elem.grammarSymbol,
        elem.codeUnit,
        elem.enumv,
        elem.math,
        elem.infinity,
        elem.simpleValue,
      ),
    )

    /** appender */
    given rule: Rule[Elem] = (app, elem) => {
      if (elem.isBottom) app >> "⊥"
      else {
        var strs = Vector[String]()
        if (!elem.clo.isBottom) strs :+= elem.clo.toString
        if (!elem.cont.isBottom) strs :+= elem.cont.toString
        if (!elem.part.isBottom) strs :+= elem.part.toString
        if (!elem.astValue.isBottom) strs :+= elem.astValue.toString
        if (!elem.grammarSymbol.isBottom) strs :+= elem.grammarSymbol.toString
        if (!elem.codeUnit.isBottom) strs :+= elem.codeUnit.toString
        if (!elem.enumv.isBottom) strs :+= elem.enumv.toString
        if (!elem.math.isBottom) strs :+= elem.math.toString
        if (!elem.infinity.isBottom) strs :+= elem.infinity.toString
        if (!elem.simpleValue.isBottom) strs :+= elem.simpleValue.toString
        app >> strs.mkString(", ")
      }
    }

    /** transfer for variadic operation */
    def vopTransfer(vop: VOp, vs: List[Elem]): Elem =
      import VOp.*
      // helpers
      def asMath(av: Elem): Option[BigDecimal] = av.getSingle match
        case Many         => exploded("vop transfer")
        case One(Math(n)) => Some(n)
        case _            => None
      def asStr(av: Elem): Option[String] = av.getSingle match
        case Many              => exploded("vop transfer")
        case One(Str(s))       => Some(s)
        case One(CodeUnit(cu)) => Some(cu.toString)
        case _                 => None
      // transfer body
      if (!vs.exists(_.isBottom)) vop match
        case Min =>
          val set = scala.collection.mutable.Set[Elem]()
          if (vs.exists(apply(NEG_INF) ⊑ _)) set += apply(NEG_INF)
          val filtered = vs.filter((v) => !(apply(POS_INF) ⊑ v))
          if (filtered.isEmpty) set += apply(POS_INF)
          set += doVopTransfer(asMath, _ min _, apply, filtered)
          set.foldLeft(Bot)(_ ⊔ _)
        case Max =>
          val set = scala.collection.mutable.Set[Elem]()
          if (vs.exists(apply(POS_INF) ⊑ _)) set += apply(POS_INF)
          val filtered = vs.filter((v) => !(apply(NEG_INF) ⊑ v))
          if (filtered.isEmpty) set += apply(NEG_INF)
          set += doVopTransfer(asMath, _ min _, apply, filtered)
          set.foldLeft(Bot)(_ ⊔ _)
        case Concat => doVopTransfer[String](asStr, _ + _, apply, vs)
      else Bot

    /** helpers for make transition for variadic operators */
    protected def doVopTransfer[T](
      f: Elem => Option[T],
      op: (T, T) => T,
      g: T => Elem,
      vs: List[Elem],
    ): Elem =
      val vst = vs.map(f).flatten
      if (vst.size != vs.size) Bot
      else g(vst.reduce(op))

    /** transfer for mathematical operation */
    def mopTransfer(mop: MOp, vs: List[Elem]): Elem = mathTop

    /** element interfaces */
    extension (elem: Elem) {

      /** get key values */
      def keyValue: Elem = apply(part = elem.part, str = elem.str)

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.clo ⊑ that.clo &&
        elem.cont ⊑ that.cont &&
        elem.part ⊑ that.part &&
        elem.astValue ⊑ that.astValue &&
        elem.grammarSymbol ⊑ that.grammarSymbol &&
        elem.codeUnit ⊑ that.codeUnit &&
        elem.enumv ⊑ that.enumv &&
        elem.math ⊑ that.math &&
        elem.infinity ⊑ that.infinity &&
        elem.simpleValue ⊑ that.simpleValue

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.clo ⊔ that.clo,
        elem.cont ⊔ that.cont,
        elem.part ⊔ that.part,
        elem.astValue ⊔ that.astValue,
        elem.grammarSymbol ⊔ that.grammarSymbol,
        elem.codeUnit ⊔ that.codeUnit,
        elem.enumv ⊔ that.enumv,
        elem.math ⊔ that.math,
        elem.infinity ⊔ that.infinity,
        elem.simpleValue ⊔ that.simpleValue,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.clo ⊓ that.clo,
        elem.cont ⊓ that.cont,
        elem.part ⊓ that.part,
        elem.astValue ⊓ that.astValue,
        elem.grammarSymbol ⊓ that.grammarSymbol,
        elem.codeUnit ⊓ that.codeUnit,
        elem.enumv ⊓ that.enumv,
        elem.math ⊓ that.math,
        elem.infinity ⊓ that.infinity,
        elem.simpleValue ⊓ that.simpleValue,
      )

      /** prune operator */
      override def --(that: Elem): Elem = Elem(
        elem.clo -- that.clo,
        elem.cont -- that.cont,
        elem.part -- that.part,
        elem.astValue -- that.astValue,
        elem.grammarSymbol -- that.grammarSymbol,
        elem.codeUnit -- that.codeUnit,
        elem.enumv -- that.enumv,
        elem.math -- that.math,
        elem.infinity -- that.infinity,
        elem.simpleValue -- that.simpleValue,
      )

      /** concretization function */
      override def gamma: BSet[AValue] =
        (elem.clo.gamma: BSet[AValue]) ⊔
        elem.cont.gamma ⊔
        elem.part.gamma ⊔
        elem.astValue.gamma ⊔
        elem.grammarSymbol.gamma ⊔
        elem.codeUnit.gamma ⊔
        elem.enumv.gamma ⊔
        elem.math.gamma ⊔
        elem.infinity.gamma ⊔
        elem.simpleValue.gamma

      /** get single value */
      override def getSingle: Flat[AValue] =
        (elem.clo.getSingle: Flat[AValue]) ⊔
        elem.cont.getSingle ⊔
        elem.part.getSingle ⊔
        elem.astValue.getSingle ⊔
        elem.grammarSymbol.getSingle ⊔
        elem.codeUnit.getSingle ⊔
        elem.enumv.getSingle ⊔
        elem.math.getSingle ⊔
        elem.infinity.getSingle ⊔
        elem.simpleValue.getSingle

      /** getters */
      def clo: AbsClo = elem.clo
      def cont: AbsCont = elem.cont
      def part: AbsPart = elem.part
      def astValue: AbsAstValue = elem.astValue
      def grammarSymbol: AbsGrammarSymbol = elem.grammarSymbol
      def codeUnit: AbsCodeUnit = elem.codeUnit
      def enumv: AbsEnum = elem.enumv
      def math: AbsMath = elem.math
      def infinity: AbsInfinity = elem.infinity
      def simpleValue: AbsSimpleValue = elem.simpleValue
      def number: AbsNumber = elem.simpleValue.number
      def bigInt: AbsBigInt = elem.simpleValue.bigInt
      def str: AbsStr = elem.simpleValue.str
      def bool: AbsBool = elem.simpleValue.bool
      def undef: AbsUndef = elem.simpleValue.undef
      def nullv: AbsNull = elem.simpleValue.nullv

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        var parts = elem.part.toSet
        for {
          AClo(_, captured) <- elem.clo
          (_, value) <- captured
        } parts ++= value.reachableParts
        for {
          ACont(_, captured) <- elem.cont
          (_, value) <- captured
        } parts ++= value.reachableParts
        parts

      /** bitwise operations */
      def &(that: Elem): Elem = ??? // TODO
      def |(that: Elem): Elem = ??? // TODO
      def ^(that: Elem): Elem = ??? // TODO

      /** comparison operations */
      def =^=(that: Elem): Elem =
        apply(bool = (elem.getSingle, that.getSingle) match
          case (Zero, _) | (_, Zero) => AbsBool.Bot
          case (One(l), One(r))      => AbsBool(Bool(l == r))
          case _                     => if ((elem ⊓ that).isBottom) AF else AB,
        )
      def ==^==(that: Elem): Elem = ??? // TODO
      def <(that: Elem): Elem = ??? // TODO

      /** logical operations */
      def &&(that: Elem): Elem = ??? // TODO
      def ||(that: Elem): Elem = ??? // TODO
      def ^^(that: Elem): Elem = ??? // TODO

      /** numeric operations */
      def +(that: Elem): Elem = ??? // TODO
      // AbsValue(
      //   str = (
      //     (left.str plus right.str) ⊔
      //       (left.str plusNum right.num)
      //   ),
      //   num = (
      //     (left.num plus right.num) ⊔
      //       (right.num plusInt left.int) ⊔
      //       (left.num plusInt right.int)
      //   ),
      //   int = left.int plus right.int,
      //   bigInt = left.bigInt plus right.bigInt,
      // )
      def sub(that: Elem): Elem = ??? // TODO
      def /(that: Elem): Elem = ??? // TODO
      def *(that: Elem): Elem = ??? // TODO
      // AbsValue(
      //   num = (
      //     (left.num mul right.num) ⊔
      //       (right.num mulInt left.int) ⊔
      //       (left.num mulInt right.int)
      //   ),
      //   int = left.int mul right.int,
      //   bigInt = left.bigInt mul right.bigInt,
      // )
      def %(that: Elem): Elem = ??? // TODO
      def %%(that: Elem): Elem = ??? // TODO
      def **(that: Elem): Elem = ??? // TODO
      def <<(that: Elem): Elem = ??? // TODO
      def >>>(that: Elem): Elem = ??? // TODO
      def >>(that: Elem): Elem = ??? // TODO

      /** unary operations */
      def unary_- : Elem = ??? // TODO
      def unary_! : Elem = apply(bool = !elem.bool)
      def unary_~ : Elem = ??? // TODO
      def abs: Elem = ??? // TODO
      def floor: Elem = ??? // TODO

      /** type operations */
      def typeOf(st: AbsState): Elem =
        var set = Set[String]()
        if (!elem.number.isBottom) set += "Number"
        if (!elem.bigInt.isBottom) set += "BigInt"
        if (!elem.str.isBottom) set += "String"
        if (!elem.bool.isBottom) set += "Boolean"
        if (!elem.undef.isBottom) set += "Undefined"
        if (!elem.nullv.isBottom) set += "Null"
        if (!elem.part.isBottom) for (part <- elem.part) {
          val tname = st.get(part).getTy
          if (cfg.tyModel.isSubTy(tname, "Object")) set += "Object"
          else if (cfg.tyModel.isSubTy(tname, "Symbol")) set += "Symbol"
          else set += "SpecType"
        }
        apply(str = AbsStr(set.map(Str.apply)))

      /** type check */
      def typeCheck(ty: Ty, st: AbsState): Elem = ???

      /** helper functions for abstract transfer */
      def convertTo(cop: COp, radix: Elem): Elem =
        import COp.*
        var newV = Bot
        for (CodeUnit(cu) <- elem.codeUnit) newV ⊔= (cop match
          case ToMath => apply(Math(cu.toInt))
          case _      => Bot
        )
        for (Math(n) <- elem.math) newV ⊔= (cop match
          case ToApproxNumber => apply(Number(n.toDouble))
          case ToNumber       => apply(Number(n.toDouble))
          case ToBigInt       => apply(BigInt(n.toBigInt))
          case ToMath         => apply(Math(n))
          case _              => Bot
        )
        for (Str(s) <- elem.str) newV ⊔= (cop match
          case ToNumber => apply(ESValueParser.str2number(s))
          case ToBigInt => apply(ESValueParser.str2bigint(s))
          case _: ToStr => apply(Str(s))
          case _        => Bot
        )
        for (Number(d) <- elem.number) newV ⊔= (cop match
          case ToMath => apply(Math(d))
          case _: ToStr =>
            radix.asInt.foldLeft(Bot)((v, n) => v ⊔ apply(toStringHelper(d, n)))
          case ToNumber => apply(Number(d))
          case ToBigInt => apply(BigInt(BigDecimal.exact(d).toBigInt))
          case _        => Bot
        )
        for (BigInt(b) <- elem.bigInt) newV ⊔= (cop match
          case ToMath => apply(Math(b))
          case _: ToStr =>
            radix.asInt.foldLeft(Bot)((v, n) => v ⊔ apply(Str(b.toString(n))))
          case ToBigInt => apply(BigInt(b))
          case _        => Bot
        )
        newV
      def sourceText: Elem = apply(str =
        AbsStr(
          elem.astValue.toList.map(x =>
            Str(x.ast.toString(grammar = Some(cfg.grammar)).trim),
          ),
        ),
      )
      def parse(rule: Elem): Elem =
        var newV: Elem = Bot
        // codes
        var codes: Set[(String, List[Boolean])] = Set()
        for (Str(s) <- elem.str) codes += (s, List())
        for (AstValue(ast) <- elem.astValue) {
          val code = ast.toString(grammar = Some(cfg.grammar))
          val args = ast match
            case syn: Syntactic => syn.args
            case _              => List()
          codes += (code, args)
        }
        // parse
        for {
          GrammarSymbol(name, params) <- rule.grammarSymbol
          (str, args) <- codes
          parseArgs = if (params.isEmpty) args else params
        } newV ⊔= apply(cfg.esParser(name, parseArgs).from(str))
        // result
        newV
      def substring(from: Elem): Elem =
        (elem.getSingle, from.getSingle) match
          case (Zero, _) | (_, Zero) => Bot
          case (Many, _) | (_, Many) => exploded("ESubstring")
          case (One(Str(s)), One(Math(f))) if f.isValidInt =>
            apply(s.substring(f.toInt))
          case _ => Bot

      def substring(from: Elem, to: Elem): Elem =
        (elem.getSingle, from.getSingle, to.getSingle) match
          case (Zero, _, _) | (_, Zero, _) | (_, _, Zero) => Bot
          case (Many, _, _) | (_, Many, _) | (_, _, Many) =>
            exploded("ESubstring")
          case (
                One(Str(s)),
                One(Math(f)),
                One(Math(t)),
              ) if f.isValidInt =>
            if (s.length < t) apply(s.substring(f.toInt))
            else if (t.isValidInt) apply(s.substring(f.toInt, t.toInt))
            else Bot
          case _ => Bot
      def trim(isStarting: Boolean): Elem = elem.getSingle match
        case Many        => exploded("ETrim")
        case One(Str(s)) => apply(trimString(s, isStarting, cfg.esParser))
        case _           => Bot
      def instanceOf(ty: Elem): Elem = ???
      def sizeOf(value: Elem): Elem = ???
      def sizeOf(st: AbsState): Elem = ???

      /** refine receiver object */
      def refineThis(func: Func): Elem = elem

      /** get syntactic SDO */
      def getSdo(method: String): List[(Func, Elem)] = ???

      /** get lexical result */
      def getLexical(method: String): Elem = ???

      /** getters */
      def ty: ValueTy = notSupported("ValueBasicDomain.ty")
      def refinements: Refinements = notSupported(
        "ValueBasicDomain.refinements",
      )

      // -------------------------------------------------------------------------
      // private helpers
      // -------------------------------------------------------------------------
      // conversion to integers
      private def asInt: Set[Int] =
        var set: Set[Int] = Set()
        for (Math(n) <- elem.math if n.isValidInt) set += n.toInt
        for (Number(n) <- elem.number if n.isValidInt) set += n.toInt
        set
    }
  }
}
