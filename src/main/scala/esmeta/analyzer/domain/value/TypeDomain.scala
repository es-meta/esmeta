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
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, Set => MSet}

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
  lazy val mathTop: Elem = Elem(MathTopT)
  lazy val simpleValueTop: Elem = Elem(???)
  lazy val numberTop: Elem = Elem(NumberTopT)
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
      if (!ty.number.isBottom) names += "Number"
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
        case COp.ToMath
            if (!ty.math.isBottom | !ty.number.isBottom | ty.bigInt) =>
          MathTopT
        case COp.ToNumber
            if (!ty.math.isBottom | !ty.str.isBottom | !ty.number.isBottom) =>
          NumberTopT
        case COp.ToBigInt
            if (!ty.math.isBottom | !ty.str.isBottom | !ty.number.isBottom | ty.bigInt) =>
          BigIntT
        case COp.ToStr(_)
            if (!ty.str.isBottom | !ty.number.isBottom | ty.bigInt) =>
          StrTopT
        case _ => ValueTy(),
      )
    def sourceText: Elem = strTop
    def parse(rule: Elem): Elem = rule.ty.grammar match
      case Inf => ???
      case Fin(set) =>
        Elem(
          ValueTy(astValue =
            AstNameTy(
              (for (grammar <- set) yield grammar.name).toSet,
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

    /** get lexical result */
    def getLexical(method: String): Elem = Elem(
      if (elem.ty.astValue.isBottom) ValueTy()
      else
        method match
          case "SV" | "TRV" | "StringValue" => StrTopT
          case "IdentifierCodePoints"       => StrTopT
          case "MV" | "NumericValue"        => NumberTopT | BigIntT
          case "TV"                         => StrTopT | UndefT
          case "BodyText" | "FlagText"      => StrTopT
          case "Contains"                   => BoolT
          case _                            => ValueTy(),
    )

    /** get syntactic SDO */
    def getSDO(method: String): List[(Func, Elem)] = elem.ty.astValue match
      case AstTopTy =>
        for {
          func <- cfg.funcs if func.isSDO
          allSdoPattern(_, newMethod) = func.name if newMethod == method
        } yield (func, Elem(AstTopT))
      case AstNameTy(names) =>
        for {
          name <- names.toList
          pair <- astSdoCache((name, method))
        } yield pair
      case AstSingleTy(name, idx, subIdx) =>
        synSdoCache((name, idx, subIdx, method))

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
    case Math(n)                   => MathT(n)
    case n: Number                 => NumberT(n)
    case BigInt(_)                 => BigIntT
    case Str(n)                    => StrT(n)
    case Bool(true)                => TrueT
    case Bool(false)               => FalseT
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
            (!l.ty.math.isBottom & !r.ty.math.isBottom) |
            (!l.ty.number.isBottom & !r.ty.number.isBottom) |
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
      case AstTopTy =>
        names ++= astChildMap.keySet ++ Set("ParseNode", "Nonterminal")
      case ty: AstNonTopTy =>
        val astNames = ty.toName.names
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

  /** sdo access helper */
  private lazy val astSdoCache: ((String, String)) => List[(Func, Elem)] =
    cached[(String, String), List[(Func, Elem)]] {
      case (name, method) =>
        val result = (for {
          (fid, thisTy, hint) <- sdoMap.getOrElse(name, Set()) if hint == method
        } yield (cfg.funcMap(fid), Elem(thisTy))).toList
        if (result.isEmpty) {
          if (defaultSdos contains method) {
            val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
            for {
              (rhs, idx) <- cfg.grammar.nameMap(name).rhsList.zipWithIndex
              subIdx <- (0 until rhs.countSubs)
            } yield (defaultFunc, Elem(AstSingleT(name, idx, subIdx)))
          } else {
            logger.warn(s"unknown syntax-directed operation: $name.$method")
            List()
          }
        } else result
    }
  private lazy val synSdoCache =
    cached[(String, Int, Int, String), List[(Func, Elem)]] {
      case (name, idx, subIdx, method) =>
        val result = (for {
          (fid, thisTy, hint) <- sdoMap.getOrElse(s"$name[$idx,$subIdx]", Set())
          if hint == method
        } yield (cfg.funcMap(fid), Elem(thisTy))).toList
        if (result.isEmpty) {
          if (defaultSdos contains method) {
            val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
            List((defaultFunc, Elem(AstSingleT(name, idx, subIdx))))
          } else {
            logger.warn(s"unknown syntax-directed operation: $name.$method")
            List()
          }
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
    val infos: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
    var defaultInfos: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
    for {
      func <- cfg.funcs if func.isSDO
      isDefaultSdo = func.name.startsWith("<DEFAULT>") if !isDefaultSdo
      sdoPattern(name, idxStr, subIdxStr, method) = func.name
      (idx, subIdx) = (idxStr.toInt, subIdxStr.toInt)
      key = s"$name[$idx,$subIdx]"
    } {
      val newInfo = (func.id, AstSingleT(name, idx, subIdx), method)
      val isDefaultSdo = defaultSdos contains method

      // update target info
      val targetInfos = if (isDefaultSdo) defaultInfos else infos
      if (targetInfos contains key) targetInfos(key) += newInfo
      else targetInfos(key) = MSet(newInfo)
      if (targetInfos contains name) targetInfos(name) += newInfo
      else targetInfos(name) = MSet(newInfo)

      // propagate chain production
      if (!isDefaultSdo) worklist += name
    }

    // record original infos
    val origInfos = (for { (k, set) <- infos } yield k -> set.toSet).toMap

    // propagate chain productions
    @tailrec
    def aux(): Unit = worklist.next match
      case Some(key) =>
        val childInfo = infos.getOrElse(key, MSet())
        for {
          next <- edges.getOrElse(key, MSet())
          info = infos.getOrElse(next, MSet())
          oldInfoSize = info.size

          newInfo =
            // A[i,j] -> A
            if (key endsWith "]") info ++ childInfo
            // A.method -> B[i,j].method
            // only if B[i,j].method not exists (chain production)
            else {
              val origInfo = origInfos.getOrElse(next, Set())
              info ++ (for {
                triple <- childInfo
                if !(origInfo.exists(_._3 == triple._3))
              } yield triple)
            }

          _ = infos(next) = newInfo
          if newInfo.size > oldInfoSize
        } worklist += next
        aux()
      case None => /* do nothing */
    aux()

    // merge default infos
    (for {
      key <- infos.keySet ++ defaultInfos.keySet
      info = infos.getOrElse(key, MSet())
      defaultInfo = defaultInfos.getOrElse(key, MSet())
      finalInfo = (info ++ defaultInfo).toSet
    } yield key -> finalInfo).toMap
  }
}
