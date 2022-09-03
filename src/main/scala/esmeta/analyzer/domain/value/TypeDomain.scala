package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.ir.{COp, Name, VOp}
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*

/** type domain for values */
object TypeDomain extends value.Domain {

  /** elements */
  case class Elem(ty: ValueTy) extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract value")

  /** bottom element */
  val Bot: Elem = Elem(ValueTy())

  /** abstraction functions */
  def alpha(vs: Iterable[AValue]): Elem = Elem(getValueTy(vs))

  /** constructor with types */
  def apply(ty: Ty): Elem = ty match
    case _: UnknownTy => Bot
    case vty: ValueTy => Elem(vty)

  /** predefined top values */
  lazy val compTop: Elem = Elem(???)
  lazy val pureValueTop: Elem = Elem(???)
  lazy val cloTop: Elem = Elem(CloTopT)
  lazy val contTop: Elem = Elem(ContTopT)
  lazy val partTop: Elem = Elem(???)
  lazy val astValueTop: Elem = Elem(???)
  lazy val grammarTop: Elem = Elem(???)
  lazy val codeUnitTop: Elem = Elem(CodeUnitT)
  lazy val constTop: Elem = Elem(???)
  lazy val mathTop: Elem = Elem(MathT)
  lazy val simpleValueTop: Elem = Elem(???)
  lazy val numberTop: Elem = Elem(NumberT)
  lazy val bigIntTop: Elem = Elem(BigIntT)
  lazy val strTop: Elem = Elem(StrTopT)
  lazy val boolTop: Elem = Elem(BoolT)
  lazy val undefTop: Elem = Elem(UndefT)
  lazy val nullTop: Elem = Elem(NullT)
  lazy val absentTop: Elem = Elem(AbsentT)

  /** constructors */
  def apply(
    comp: AbsComp = ???,
    pureValue: AbsPureValue = ???,
    clo: AbsClo = ???,
    cont: AbsCont = ???,
    part: AbsPart = ???,
    astValue: AbsAstValue = ???,
    grammar: AbsGrammar = ???,
    codeUnit: AbsCodeUnit = ???,
    const: AbsConst = ???,
    math: AbsMath = ???,
    simpleValue: AbsSimpleValue = ???,
    num: AbsNumber = ???,
    bigInt: AbsBigInt = ???,
    str: AbsStr = ???,
    bool: AbsBool = ???,
    undef: AbsUndef = ???,
    nullv: AbsNull = ???,
    absent: AbsAbsent = ???,
  ): Elem = ???

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = ???

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    import TyStringifier.given
    app >> elem.ty

  /** transfer for variadic operation */
  def vopTransfer(vop: VOp, vs: List[Elem]): Elem = ???

  /** element interfaces */
  extension (elem: Elem) {

    /** get key values */
    def keyValue: Elem = ???

    /** partial order */
    def ⊑(that: Elem): Boolean = elem.ty <= that.ty

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(elem.ty | that.ty)

    /** meet operator */
    override def ⊓(that: Elem): Elem = Elem(elem.ty & that.ty)

    /** prune operator */
    override def --(that: Elem): Elem = Elem(elem.ty -- that.ty)

    /** concretization function */
    override def gamma: BSet[AValue] = Inf

    /** get single value */
    override def getSingle: Flat[AValue] = Many

    /** get reachable address partitions */
    def reachableParts: Set[Part] = Set()

    /** bitwise operations */
    def &(that: Elem): Elem = ???
    def |(that: Elem): Elem = ???
    def ^(that: Elem): Elem = ???

    /** comparison operations */
    def =^=(that: Elem): Elem = ???
    def ==^==(that: Elem): Elem = ???
    def <(that: Elem): Elem = ???

    /** logical operations */
    def &&(that: Elem): Elem = logicalOp(_ && _)(elem, that)
    def ||(that: Elem): Elem = logicalOp(_ || _)(elem, that)
    def ^^(that: Elem): Elem = logicalOp(_ ^ _)(elem, that)

    /** numeric operations */
    def +(that: Elem): Elem = ???
    def sub(that: Elem): Elem = ???
    def /(that: Elem): Elem = ???
    def *(that: Elem): Elem = ???
    def %(that: Elem): Elem = ???
    def %%(that: Elem): Elem = ???
    def **(that: Elem): Elem = ???
    def <<(that: Elem): Elem = ???
    def >>>(that: Elem): Elem = ???
    def >>(that: Elem): Elem = ???

    /** unary operations */
    def unary_- : Elem = ???
    def unary_! : Elem = ???
    def unary_~ : Elem = ???
    def abs: Elem = ???
    def floor: Elem = ???

    /** type operations */
    def typeOf(st: AbsState): Elem = ???

    /** type check */
    def typeCheck(tname: String, st: AbsState): Elem = ???

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: Elem): Elem = ???
    def sourceText: Elem = ???
    def parse(rule: Elem): Elem = ???
    def duplicated(st: AbsState): Elem = ???
    def substring(from: Elem): Elem = ???
    def substring(from: Elem, to: Elem): Elem = ???
    def clamp(lower: Elem, upper: Elem): Elem = ???
    def isArrayIndex: Elem = ???

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem = ???
    def pruneTypeCheck(tname: String, positive: Boolean): Elem = ???
    def pruneValue(r: Elem, positive: Boolean): Elem = ???

    /** completion helpers */
    def wrapCompletion: Elem = ???
    def wrapCompletion(ty: String): Elem = ???
    def unwrapCompletion: Elem = ???
    def isCompletion: Elem = ???
    def abruptCompletion: Elem = ???

    /** absent helpers */
    def removeAbsent: Elem = ???
    def isAbsent: Elem = ???

    /** refine receiver object */
    def refineThis(func: Func): Elem = ???

    /** getters */
    def comp: AbsComp = ???
    def pureValue: AbsPureValue = ???
    def clo: AbsClo = ???
    def cont: AbsCont = ???
    def part: AbsPart = ???
    def astValue: AbsAstValue = ???
    def grammar: AbsGrammar = ???
    def codeUnit: AbsCodeUnit = ???
    def const: AbsConst = ???
    def math: AbsMath = ???
    def simpleValue: AbsSimpleValue = ???
    def number: AbsNumber = ???
    def bigInt: AbsBigInt = ???
    def str: AbsStr = ???
    def bool: AbsBool = ???
    def undef: AbsUndef = ???
    def nullv: AbsNull = ???
    def absent: AbsAbsent = ???
    def toTy: ValueTy = elem.ty
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // value type getter
  private def getValueTy(vs: Iterable[AValue]): ValueTy =
    vs.foldLeft(ValueTy()) { case (vty, v) => vty | getValueTy(v) }

  // value type getter
  private def getValueTy(v: AValue): ValueTy = v match
    case AComp(CONST_NORMAL, v, _) => NormalT(getValueTy(v))
    case _: AComp                  => AbruptT
    case AClo(func, _)             => CloT(func.name)
    case ACont(target, _)          => ContT(target.func.name)
    case AstValue(ast)             => AstT(ast.name)
    case grammar: Grammar          => GrammarT(grammar)
    case CodeUnit(_)               => CodeUnitT
    case Const(name)               => ConstT(name)
    case Math(n)                   => MathT
    case Number(_)                 => NumberT
    case BigInt(_)                 => BigIntT
    case Str(n)                    => StrT(n)
    case Bool(true)                => TrueT
    case Bool(false)               => BoolT
    case Undef                     => UndefT
    case Null                      => NullT
    case Absent                    => AbsentT
    case v => notSupported(s"impossible to convert to pure type ($v)")

  // logical operator helper
  private def logicalOp(
    op: (Boolean, Boolean) => Boolean,
  ): (Elem, Elem) => Elem = (l, r) =>
    Elem(ValueTy(bool = for {
      x <- l.ty.bool
      y <- r.ty.bool
    } yield op(x, y)))
}
