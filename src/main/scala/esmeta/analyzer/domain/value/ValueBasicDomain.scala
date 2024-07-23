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
      comp: AbsComp = AbsComp.Bot,
      pureValue: AbsPureValue = AbsPureValue.Bot,
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract value")

    /** bottom element */
    val Bot: Elem = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[AValue]): Elem = Elem(
      AbsComp(xs.collect { case x: AComp => x }),
      AbsPureValue(xs.collect { case x: APureValue => x }),
    )

    /** constructor with types */
    def apply(ty: Ty, refinements: Refinements) = Top

    /** constructor for completions */
    def createCompletion(
      ty: AbsValue,
      value: AbsValue,
      target: AbsValue,
    ): Elem = Elem(AbsComp(ty, value, target))

    /** predefined top values */
    def compTop: Elem = Bot.copy(comp = AbsComp.Top)
    def pureValueTop: Elem = Bot.copy(pureValue = AbsPureValue.Top)
    val cloTop: Elem = Bot.copy(pureValue = AbsPureValue.cloTop)
    val contTop: Elem = Bot.copy(pureValue = AbsPureValue.contTop)
    val partTop: Elem = Bot.copy(pureValue = AbsPureValue.partTop)
    val astValueTop: Elem = Bot.copy(pureValue = AbsPureValue.astValueTop)
    val ntTop: Elem = Bot.copy(pureValue = AbsPureValue.ntTop)
    val codeUnitTop: Elem = Bot.copy(pureValue = AbsPureValue.codeUnitTop)
    val enumTop: Elem = Bot.copy(pureValue = AbsPureValue.enumTop)
    val mathTop: Elem = Bot.copy(pureValue = AbsPureValue.mathTop)
    val infinityTop: Elem = Bot.copy(pureValue = AbsPureValue.infinityTop)
    val simpleValueTop: Elem = Bot.copy(pureValue = AbsPureValue.simpleValueTop)
    val numberTop: Elem = Bot.copy(pureValue = AbsPureValue.numberTop)
    val bigIntTop: Elem = Bot.copy(pureValue = AbsPureValue.bigIntTop)
    val strTop: Elem = Bot.copy(pureValue = AbsPureValue.strTop)
    val boolTop: Elem = Bot.copy(pureValue = AbsPureValue.boolTop)
    val undefTop: Elem = Bot.copy(pureValue = AbsPureValue.undefTop)
    val nullTop: Elem = Bot.copy(pureValue = AbsPureValue.nullTop)
    val absentTop: Elem = Bot.copy(pureValue = AbsPureValue.absentTop)

    /** constructors */
    def apply(
      comp: AbsComp = AbsComp.Bot,
      pureValue: AbsPureValue = AbsPureValue.Bot,
      clo: AbsClo = AbsClo.Bot,
      cont: AbsCont = AbsCont.Bot,
      part: AbsPart = AbsPart.Bot,
      astValue: AbsAstValue = AbsAstValue.Bot,
      nt: AbsNt = AbsNt.Bot,
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
      absent: AbsAbsent = AbsAbsent.Bot,
    ): Elem = Elem(
      comp,
      pureValue ⊔ AbsPureValue(
        clo,
        cont,
        part,
        astValue,
        nt,
        codeUnit,
        enumv,
        math,
        infinity,
        simpleValue ⊔ AbsSimpleValue(
          num,
          bigInt,
          str,
          bool,
          undef,
          nullv,
          absent,
        ),
      ),
    )

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = Some(
      (
        elem.comp,
        elem.pureValue,
      ),
    )

    /** appender */
    given rule: Rule[Elem] = (app, elem) =>
      if (!elem.isBottom) {
        val Elem(comp, pureValue) = elem
        var strs = Vector[String]()
        if (!comp.isBottom) strs :+= comp.toString
        if (!pureValue.isBottom) strs :+= pureValue.toString
        app >> strs.mkString(", ")
      } else app >> "⊥"

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
      def keyValue: Elem = apply(part = part, str = str)

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.comp ⊑ that.comp &&
        elem.pureValue ⊑ that.pureValue

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.comp ⊔ that.comp,
        elem.pureValue ⊔ that.pureValue,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.comp ⊓ that.comp,
        elem.pureValue ⊓ that.pureValue,
      )

      /** prune operator */
      override def --(that: Elem): Elem = Elem(
        elem.comp -- that.comp,
        elem.pureValue -- that.pureValue,
      )

      /** concretization function */
      override def gamma: BSet[AValue] =
        elem.comp.gamma ⊔
        elem.pureValue.gamma

      /** get single value */
      override def getSingle: Flat[AValue] =
        elem.comp.getSingle ⊔
        elem.pureValue.getSingle

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        comp.reachableParts ++ pureValue.reachableParts

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
      def typeCheck(tname: String, st: AbsState): Elem =
        var bv: AbsBool = AbsBool.Bot
        if (!elem.number.isBottom) bv ⊔= AbsBool(Bool(tname == "Number"))
        if (!elem.bigInt.isBottom) bv ⊔= AbsBool(Bool(tname == "BigInt"))
        if (!elem.str.isBottom) bv ⊔= AbsBool(Bool(tname == "String"))
        if (!elem.bool.isBottom) bv ⊔= AbsBool(Bool(tname == "Boolean"))
        if (!elem.enumv.isBottom)
          bv ⊔= AbsBool(Bool(tname == "Enum"))
        if (!elem.comp.isBottom)
          bv ⊔= AbsBool(Bool(tname == "CompletionRecord"))
        if (!elem.undef.isBottom)
          bv ⊔= AbsBool(Bool(tname == "Undefined"))
        if (!elem.nullv.isBottom) bv ⊔= AbsBool(Bool(tname == "Null"))
        if (!elem.clo.isBottom)
          bv ⊔= AbsBool(Bool(tname == "AbstractClosure"))
        elem.astValue.getSingle match
          case Zero => /* do nothing */
          case Many => bv = AB
          case One(AstValue(ast)) =>
            bv ⊔= AbsBool(
              Bool(tname == "ParseNode" || (ast.types contains tname)),
            )
        for (part <- elem.part) {
          val newTName = st.get(part).getTy
          bv ⊔= AbsBool(
            Bool(
              newTName == tname || cfg.tyModel.isSubTy(newTName, tname),
            ),
          )
        }
        apply(bool = bv)

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
          Nt(name, params) <- rule.nt
          (str, args) <- codes
          parseArgs = if (params.isEmpty) args else params
        } newV ⊔= apply(cfg.esParser(name, parseArgs).from(str))
        // result
        newV
      def duplicated(st: AbsState): Elem =
        apply(bool = elem.part.foldLeft(AbsBool.Bot: AbsBool) {
          case (avb, part) => avb ⊔ st.get(part).duplicated
        })
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

      /** completion helpers */
      def wrapCompletion: Elem = wrapCompletion("normal")
      def wrapCompletion(ty: String): Elem = apply(comp = {
        if (!pureValue.isBottom)
          comp ⊔ AbsComp(
            Map(ty -> AbsComp.Result(pureValue, AbsPureValue(ENUM_EMPTY))),
          )
        else comp
      })
      def unwrapCompletion: Elem =
        Elem(pureValue = comp.normal.value ⊔ elem.pureValue)
      def isCompletion: Elem =
        var b: AbsBool = AbsBool.Bot
        if (!comp.isBottom) b ⊔= AT
        if (!pureValue.isBottom) b ⊔= AF
        apply(bool = b)
      def normalCompletion: Elem =
        if (pureValue.isBottom) Bot
        else
          val res = AbsComp.Result(pureValue, AbsPureValue(ENUM_EMPTY))
          Elem(comp = AbsComp(Map("normal" -> res)))
      def abruptCompletion: Elem = apply(comp = comp.removeNormal)

      /** absent helpers */
      def removeAbsent: Elem = elem -- absentTop
      def isAbsent: Elem =
        var b: AbsBool = AbsBool.Bot
        if (!absent.isBottom) b ⊔= AT
        if (!removeAbsent.isBottom) b ⊔= AF
        apply(bool = b)

      /** refine receiver object */
      def refineThis(func: Func): Elem = elem

      /** get syntactic SDO */
      def getSdo(method: String): List[(Func, Elem)] = ???

      /** get lexical result */
      def getLexical(method: String): Elem = ???

      /** getters */
      def comp: AbsComp = elem.comp
      def pureValue: AbsPureValue = elem.pureValue
      def clo: AbsClo = elem.pureValue.clo
      def cont: AbsCont = elem.pureValue.cont
      def part: AbsPart = elem.pureValue.part
      def astValue: AbsAstValue = elem.pureValue.astValue
      def nt: AbsNt = elem.pureValue.nt
      def codeUnit: AbsCodeUnit = elem.pureValue.codeUnit
      def enumv: AbsEnum = elem.pureValue.enumv
      def math: AbsMath = elem.pureValue.math
      def infinity: AbsInfinity = elem.pureValue.infinity
      def simpleValue: AbsSimpleValue = elem.pureValue.simpleValue
      def number: AbsNumber = elem.pureValue.number
      def bigInt: AbsBigInt = elem.pureValue.bigInt
      def str: AbsStr = elem.pureValue.str
      def bool: AbsBool = elem.pureValue.bool
      def undef: AbsUndef = elem.pureValue.undef
      def nullv: AbsNull = elem.pureValue.nullv
      def absent: AbsAbsent = elem.pureValue.absent
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
