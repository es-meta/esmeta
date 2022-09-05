package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.ir.{COp, Name, VOp}
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.spec.{Grammar => _, *}
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

  /** constructor for completions */
  def createCompletion(
    ty: AbsValue,
    value: AbsValue,
    target: AbsValue,
  ): Elem =
    val consts = ty.ty.const
    val normal =
      if (consts contains "normal") value.ty.pureValue
      else PureValueTy()
    val abrupt = !(consts - "normal").isEmpty
    Elem(ValueTy(normal = normal, abrupt = abrupt))

  /** predefined top values */
  lazy val compTop: Elem = Elem(???)
  lazy val pureValueTop: Elem = Elem(???)
  lazy val cloTop: Elem = Elem(CloTopT)
  lazy val contTop: Elem = Elem(ContTopT)
  lazy val partTop: Elem = Elem(???)
  lazy val astValueTop: Elem = Elem(AstTopT)
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
    comp: AbsComp,
    pureValue: AbsPureValue,
    clo: AbsClo,
    cont: AbsCont,
    part: AbsPart,
    astValue: AbsAstValue,
    grammar: AbsGrammar,
    codeUnit: AbsCodeUnit,
    const: AbsConst,
    math: AbsMath,
    simpleValue: AbsSimpleValue,
    num: AbsNumber,
    bigInt: AbsBigInt,
    str: AbsStr,
    bool: AbsBool,
    undef: AbsUndef,
    nullv: AbsNull,
    absent: AbsAbsent,
  ): Elem = Top

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = None

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    import TyStringifier.given
    app >> elem.ty

  /** transfer for variadic operation */
  def vopTransfer(vop: VOp, vs: List[Elem]): Elem = vop match
    case VOp.Min | VOp.Max => mathTop
    case VOp.Concat        => strTop

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
    override def getSingle: Flat[AValue] = elem.ty.getSingle

    /** get reachable address partitions */
    def reachableParts: Set[Part] = Set()

    /** bitwise operations */
    def &(that: Elem): Elem = bitwiseOp(elem, that)
    def |(that: Elem): Elem = bitwiseOp(elem, that)
    def ^(that: Elem): Elem = bitwiseOp(elem, that)

    /** comparison operations */
    def =^=(that: Elem): Elem =
      (elem.getSingle, that.getSingle) match
        case (Zero, _) | (_, Zero)       => Bot
        case (One(l), One(r))            => Elem(BoolT(l == r))
        case _ if (elem ⊓ that).isBottom => Elem(FalseT)
        case _                           => boolTop
    def ==^==(that: Elem): Elem = numericComapreOP(elem, that)
    def <(that: Elem): Elem = numericComapreOP(elem, that)

    /** logical operations */
    def &&(that: Elem): Elem = logicalOp(_ && _)(elem, that)
    def ||(that: Elem): Elem = logicalOp(_ || _)(elem, that)
    def ^^(that: Elem): Elem = logicalOp(_ ^ _)(elem, that)

    /** numeric operations */
    def +(that: Elem): Elem = numericOp(elem, that)
    def sub(that: Elem): Elem = numericOp(elem, that)
    def /(that: Elem): Elem = numericOp(elem, that)
    def *(that: Elem): Elem = numericOp(elem, that)
    def %(that: Elem): Elem = numericOp(elem, that)
    def %%(that: Elem): Elem = numericOp(elem, that)
    def **(that: Elem): Elem = numericOp(elem, that)
    def <<(that: Elem): Elem = ???
    def >>>(that: Elem): Elem = ???
    def >>(that: Elem): Elem = ???

    /** unary operations */
    def unary_- : Elem = numericUnaryOp(elem)
    def unary_! : Elem = numericUnaryOp(elem)
    def unary_~ : Elem = logicalUnaryOp(!_)(elem)
    def abs: Elem = Elem(ValueTy(math = elem.ty.math))
    def floor: Elem = Elem(ValueTy(math = elem.ty.math))

    /** type operations */
    def typeOf(st: AbsState): Elem =
      val ty = elem.ty
      var names: Set[String] = Set()
      if (ty.names.exists(cfg.tyModel.isSubTy(_, "Object"))) names += "Object"
      if (ty.symbol) names += "Symbol"
      if (ty.number) names += "Number"
      if (ty.bigInt) names += "BigInt"
      if (!ty.str.isBottom) names += "String"
      if (!ty.bool.isEmpty) names += "Boolean"
      if (ty.undef) names += "Undefined"
      if (ty.nullv) names += "Null"
      Elem(StrT(names))

    /** type check */
    def typeCheck(tname: String, st: AbsState): Elem =
      val names = instanceNameSet(elem.ty)
      if (names.isEmpty) Bot
      else if (names == Set(tname)) Elem(TrueT)
      else if (!names.contains(tname)) Elem(FalseT)
      else boolTop

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: Elem): Elem =
      val ty = elem.ty
      Elem(cop match
        case COp.ToMath if (ty.math | ty.number | ty.bigInt)          => MathT
        case COp.ToNumber if (ty.math | !ty.str.isBottom | ty.number) => NumberT
        case COp.ToBigInt
            if (ty.math | !ty.str.isBottom | ty.number | ty.bigInt) =>
          BigIntT
        case COp.ToStr(_) if (!ty.str.isBottom | ty.number | ty.bigInt) =>
          StrTopT
        case _ => ValueTy(),
      )
    def sourceText: Elem = strTop
    def parse(rule: Elem): Elem = Elem(
      ValueTy(astValue =
        Fin(
          (for (grammar <- rule.grammar) yield grammar.name).toSet,
        ),
      ),
    )
    def duplicated(st: AbsState): Elem = boolTop
    def substring(from: Elem): Elem = strTop
    def substring(from: Elem, to: Elem): Elem = strTop
    def clamp(lower: Elem, upper: Elem): Elem = mathTop
    def isArrayIndex: Elem = boolTop

    /** TODO prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem = elem
    def pruneTypeCheck(tname: String, positive: Boolean): Elem = elem
    def pruneValue(r: Elem, positive: Boolean): Elem = elem

    /** completion helpers */
    def wrapCompletion: Elem =
      val ty = elem.ty
      Elem(ValueTy(normal = ty.normal | ty.pureValue, abrupt = ty.abrupt))
    def unwrapCompletion: Elem =
      val ty = elem.ty
      Elem(ValueTy(pureValue = ty.normal | ty.pureValue))
    def isCompletion: Elem =
      val ty = elem.ty
      var bs: Set[Boolean] = Set()
      if (!ty.comp.isBottom) bs += true
      if (!ty.pureValue.isBottom) bs += false
      Elem(ValueTy(bool = bs))
    def abruptCompletion: Elem = Elem(ValueTy(abrupt = elem.ty.abrupt))

    /** absent helpers */
    def removeAbsent: Elem = Elem(elem.ty -- AbsentT)
    def isAbsent: Elem =
      var bs: Set[Boolean] = Set()
      if (elem.ty.absent) bs += true
      if (!elem.removeAbsent.ty.isBottom) bs += false
      Elem(BoolT(bs))

    /** refine receiver object */
    def refineThis(func: Func): Elem = elem

    /** getters */
    def comp: AbsComp = ???
    def pureValue: AbsPureValue = ???
    def clo: AbsClo = ty.clo match
      case Inf => AbsClo.Top
      case Fin(set) =>
        AbsClo(for {
          name <- set.toList
        } yield AClo(cfg.fnameMap(name), Map())) // TODO captured
    def cont: AbsCont = ty.cont match
      case Inf => AbsCont.Top
      case Fin(set) =>
        AbsCont(for {
          fid <- set.toList
          node = cfg.nodeMap(fid)
          func = cfg.funcOf(node)
        } yield ACont(NodePoint(func, node, View()), Map())) // TODO captured
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
    def ty: ValueTy = elem.ty
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
    case ACont(target, _)          => ContT(target.node.id)
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

  // bitwise operator helper
  private lazy val bitwiseOp: (Elem, Elem) => Elem = (l, r) =>
    Elem(
      ValueTy(
        math = l.ty.math & r.ty.math,
        bigInt = l.ty.bigInt & r.ty.bigInt,
      ),
    )

  // logical unary operator helper
  private def logicalUnaryOp(
    op: Boolean => Boolean,
  ): Elem => Elem = b => Elem(ValueTy(bool = for (x <- b.ty.bool) yield op(x)))
  // logical operator helper
  private def logicalOp(
    op: (Boolean, Boolean) => Boolean,
  ): (Elem, Elem) => Elem = (l, r) =>
    Elem(ValueTy(bool = for {
      x <- l.ty.bool
      y <- r.ty.bool
    } yield op(x, y)))

  // numeric comparison operator helper
  private lazy val numericComapreOP: (Elem, Elem) => Elem = (l, r) =>
    Elem(
      ValueTy(
        bool =
          if (
            (l.ty.math & r.ty.math) |
            (l.ty.number & r.ty.number) |
            (l.ty.bigInt & r.ty.bigInt)
          ) Set(true, false)
          else Set(),
      ),
    )

  // numeric unary operator helper
  private lazy val numericUnaryOp: Elem => Elem = x =>
    Elem(
      ValueTy(
        math = x.ty.math,
        number = x.ty.number,
        bigInt = x.ty.bigInt,
      ),
    )

  // numeric operator helper
  private lazy val numericOp: (Elem, Elem) => Elem = (l, r) =>
    Elem(
      ValueTy(
        math = l.ty.math & r.ty.math,
        number = l.ty.number & r.ty.number,
        bigInt = l.ty.bigInt & r.ty.bigInt,
      ),
    )

  /** instance name */
  private def instanceNameSet(ty: ValueTy): Set[String] =
    var names: Set[String] = Set()
    for (name <- ty.names)
      names ++= cfg.tyModel.subTys.getOrElse(name, Set(name))
      names ++= ancestors(name)
    if (!ty.astValue.isBottom) ty.astValue match
      case Inf =>
        names ++= astChildMap.keySet ++ Set("ParseNode", "Nonterminal")
      case Fin(astNames) =>
        names += "ParseNode"
        for (astName <- astNames)
          if (!cfg.grammar.lexicalNames.contains(astName))
            names += "Nonterminal"
          names ++= astChildMap.getOrElse(astName, Set(astName))
    names

  /** get ancestor types */
  private def ancestors(tname: String): Set[String] =
    ancestorList(tname).toSet
  private def ancestorList(tname: String): List[String] =
    tname :: parent(tname).map(ancestorList).getOrElse(Nil)

  /** get parent types */
  private def parent(name: String): Option[String] = for {
    TyInfo(parent, _, _) <- cfg.tyModel.infos.get(name)
    p <- parent
  } yield p

  /** ast type check helper */
  lazy val astDirectChildMap: Map[String, Set[String]] =
    (cfg.grammar.prods.map {
      case Production(lhs, _, _, rhsList) =>
        val name = lhs.name
        val subs = rhsList.collect {
          case Rhs(_, List(Nonterminal(name, _, _)), _) => name
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
}
