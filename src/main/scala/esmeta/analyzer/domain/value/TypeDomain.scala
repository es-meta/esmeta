package esmeta.analyzer.domain

import esmeta.cfg.Func
import esmeta.interp.*
import esmeta.ir.{COp, Name}
import esmeta.js.Ast
import esmeta.util.Appender
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** abstract values for type analysis */
object TypeDomain extends ValueDomain {

  /** bottom element */
  val Bot = Elem(Set())

  /** abstraction functions */
  def apply(ast: Ast): Elem = Elem(AstT(ast.name))
  def apply(num: Number): Elem = Elem(NumberSingleT(num.n))
  def apply(num: Double): Elem = Elem(NumberSingleT(num))
  def apply(bigint: BigInt): Elem = Elem(BigIntSingleT(bigint.n))
  def apply(str: String): Elem = Elem(StrSingleT(str))
  def apply(bool: Boolean): Elem = Elem(BoolSingleT(bool))
  def apply(d: BigDecimal): Elem = math
  lazy val codeunit: Elem = Elem(CodeUnitT)
  lazy val math: Elem = Elem(MathT)
  lazy val num: Elem = Elem(NumberT)
  lazy val bigint: Elem = Elem(BigIntT)
  lazy val str: Elem = Elem(StrT)
  lazy val bool: Elem = Elem(BoolT)
  lazy val undef: Elem = Elem(UndefT)
  lazy val nullv: Elem = Elem(NullT)
  lazy val absent: Elem = Elem(AbsentT)
  lazy val abruptComp: Elem = Elem(AbruptT)
  lazy val AVF = Elem(BoolSingleT(false))
  lazy val AVT = Elem(BoolSingleT(true))

  /** constructors */
  def apply(value: AValue): Elem = Elem(Type.from(value))
  def apply(tys: Type*): Elem = Elem(tys.toSet)
  def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem = ???

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    given tyRule: Rule[Type] = (app, ty) => app >> ty.toString
    given setRule: Rule[Iterable[Type]] = iterableRule("(", " | ", ")")
    elem.set.size match
      case 0 => app >> "⊥"
      case 1 => app >> elem.set.head
      case _ => app >> elem.set

  /** sdo access helper */
  private lazy val allSdoCache: ((String, String)) => List[(Func, Elem)] =
    cached[(String, String), List[(Func, Elem)]] {
      case (name, method) =>
        for {
          (rhs, idx) <- cfg.grammar.nameMap(name).rhsList.zipWithIndex
          subIdx <- (0 until rhs.countSubs)
          pair <- sdoCache((name, idx, subIdx, method))
        } yield pair
    }
  private lazy val sdoCache =
    cached[(String, Int, Int, String), List[(Func, Elem)]] {
      case (name, idx, subIdx, method) =>
        cfg.fnameMap.get(s"$name[$idx,$subIdx].$method") match
          case Some(f) => List((f, apply(SyntacticT(name, idx, subIdx))))
          case None    =>
            // handle chain production
            val rhs = cfg.grammar.nameMap(name).rhsList(idx)
            rhs.getNts(subIdx) match
              case List(Some(chain)) => allSdoCache((chain, method))
              case _ => ??? // TODO warning missing chain production
    }

  /** elements */
  object Elem:
    def apply(tys: Type*): Elem = apply(tys)
    def apply(tys: Iterable[Type]): Elem = Elem(tys.toSet).norm
  case class Elem(set: Set[Type]) extends ValueElemTrait {

    /** getters */
    def loc: AbsLoc = ???
    def getKeyValue: Elem = notSupported(this, "getKeyValue")
    def getClos: List[(Func, Map[Name, Elem])] = for {
      CloT(fname) <- set.toList // TODO captured
    } yield (cfg.fnameMap(fname), Map())
    def getCont: List[ACont] = List() // TODO
    def getTypedArguments: List[(Elem, Type)] =
      set.toList.map(ty => (Elem(ty), ty)) // XXX upcasting?

    /** get lexical result */
    def getLexical(method: String): Elem = Elem(for {
      (ast: AstTBase) <- set if cfg.grammar.lexicalNames contains ast.name
      ty <- (ast.name, method) match {
        case (
              "IdentifierName \\ (ReservedWord)" | "IdentifierName",
              "StringValue",
            ) =>
          Set(StrT)
        case ("PrivateIdentifier", "StringValue")      => Set(StrT)
        case ("NumericLiteral", "MV" | "NumericValue") => Set(NumberT, BigIntT)
        case ("StringLiteral", "SV" | "StringValue")   => Set(StrT)
        case ("NoSubstitutionTemplate", "TV")          => Set(StrT, UndefT)
        case ("TemplateHead", "TV")                    => Set(StrT, UndefT)
        case ("TemplateMiddle", "TV")                  => Set(StrT, UndefT)
        case ("TemplateTail", "TV")                    => Set(StrT, UndefT)
        case ("NoSubstitutionTemplate", "TRV")         => Set(StrT)
        case ("TemplateHead", "TRV")                   => Set(StrT)
        case ("TemplateMiddle", "TRV")                 => Set(StrT)
        case ("TemplateTail", "TRV")                   => Set(StrT)
        case ("RegularExpressionLiteral", "BodyText" | "FlagText") => Set(StrT)
        case (_, "Contains") => Set(BoolSingleT(false))
        case _ =>
          println((ast.name, method))
          ??? // TODO
      }
    } yield ty)

    /** get syntactic SDO */
    def getSDO(method: String): List[(Func, Elem)] = for {
      ty <- set.toList
      pair <- ty match
        case ast: AstTBase if !(cfg.grammar.lexicalNames contains ast.name) =>
          ast match
            case AstT(name) => allSdoCache((name, method))
            case SyntacticT(name, idx, subIdx) =>
              sdoCache((name, idx, subIdx, method))
        case _ => List()
    } yield pair

    /** partial order */
    override def isBottom: Boolean = set.isEmpty
    def ⊑(that: Elem): Boolean = (
      (this.set subsetOf that.set) ||
        this.set.forall(_.ancestors.exists(that.set contains _))
    )

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(this.set ++ that.set).norm

    /** meet operator */
    def ⊓(that: Elem): Elem = if (this == that) this
    else
      Elem(
        this.set.filter(_.ancestors.exists(that.set contains _)) ++
        that.set.filter(_.ancestors.exists(this.set contains _)),
      )

    /** minus operator */
    def -(that: Elem): Elem =
      that.set.map(this - _).foldLeft(this)(_ ⊓ _)
    def -(that: Type): Elem = Elem(for {
      x <- set
      y <- x.bases
      t <- y - that
    } yield t).norm

    /** bitwise operations */
    // TODO
    def &(that: Elem): Elem = ???
    def |(that: Elem): Elem = ???
    def ^(that: Elem): Elem = ???

    /** comparison operations */
    def =^=(that: Elem): Elem =
      (this.set.toList, that.set.toList) match
        case (Nil, Nil)                           => Bot
        case (List(l: SingleT), List(r: SingleT)) => apply(l == r)
        case _ if (this ⊓ that).isBottom          => AVF
        case _                                    => bool
    def ==^==(that: Elem): Elem = ???
    def <(that: Elem): Elem = boolNumericOps(that)

    /** logical operations */
    def &&(that: Elem): Elem = this logicalOps that
    def ||(that: Elem): Elem = this logicalOps that
    def ^^(that: Elem): Elem = this logicalOps that

    /** numeric operations */
    def +(that: Elem): Elem = numericOps(that)
    def sub(that: Elem): Elem = numericOps(that)
    def /(that: Elem): Elem = numericOps(that)
    def *(that: Elem): Elem = numericOps(that)
    def %(that: Elem): Elem = numericOps(that)
    def %%(that: Elem): Elem = numericOps(that)
    def **(that: Elem): Elem = numericOps(that)
    // TODO
    def <<(that: Elem): Elem = ???
    def >>>(that: Elem): Elem = ???
    def >>(that: Elem): Elem = ???

    /** unary operations */
    // TODO
    def unary_- : Elem = ???
    def unary_! : Elem =
      assertBool
      if (set contains BoolT) bool
      else if (AVF ⊑ this) AVT
      else if (AVT ⊑ this) AVF
      else Bot
    def unary_~ : Elem = ???
    def abs: Elem = ???
    def floor: Elem = ???

    /** type operations */
    def typeOf(st: AbsState): Elem = Elem(for {
      t <- set
      y <- t.typeNameSet
    } yield StrSingleT(y)).norm
    def typeCheck(tname: String, st: AbsState): Elem =
      val names = for {
        ty <- set
        name <- ty.instanceNameSet
      } yield name
      if (names.isEmpty) Bot
      else if (names == Set(tname)) AVT
      else if (!names.contains(tname)) AVF
      else bool

    /** helper functions for abstract transfer */
    def convert(cop: COp, radix: Elem): Elem = ??? // TODO
    def sourceText: Elem = str
    def parse(rule: Elem): Elem =
      Elem(for { GrammarT(name) <- rule.set } yield AstT(name))
    def duplicated(st: AbsState): Elem = ??? // TODO

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem =
      r.set.headOption match
        case Some(StrSingleT(tname)) if r.set.size == 1 =>
          val ty = tname match
            case "Object"    => NameT("Object")
            case "Symbol"    => SymbolT
            case "Number"    => NumberT
            case "BigInt"    => BigIntT
            case "String"    => StrT
            case "Boolean"   => BoolT
            case "Undefined" => UndefT
            case "Null"      => NullT
            case _           => ??? // TODO
          if (positive) this ⊓ Elem(ty) else this - ty
        case _ => ??? // TODO
    def pruneTypeCheck(tname: String, positive: Boolean): Elem =
      val nameT = NameT(tname)
      val astT = AstT(tname)
      val isAst = Type.astChildMap.keySet contains tname
      val prevAstT = Elem(
        Type.astChildMap.getOrElse(tname, Set()).map(AstT(_): Type),
      )
      (positive, isAst) match
        case (false, false) => this - nameT
        case (false, true)  => (this - astT) ⊔ (prevAstT - astT)
        case (true, false)  => this ⊓ Elem(nameT)
        case (true, true)   => prevAstT ⊓ Elem(astT)
    def pruneValue(r: Elem, positive: Boolean): Elem =
      if (positive) this ⊓ r else this - r

    /** singleton */
    def getSingle: Flat[AValue] = this.set.headOption match
      case None                                    => FlatBot
      case Some(ty: SingleT) if this.set.size == 1 => FlatElem(AValue.from(ty))
      case _                                       => FlatTop

    /** get reachable locations */
    def reachableLocs: Set[Loc] = Set()

    /** completion helpers */
    def wrapCompletion: Elem = Elem(set.map(_.wrapCompletion)).norm
    def unwrapCompletion: Elem = Elem(for {
      ty <- set
      pureTy <- ty match
        case NormalT(p)  => Some(p)
        case AbruptT     => None
        case p: PureType => Some(p)
        case TopT        => ??? // TODO
    } yield pureTy).norm
    def isCompletion: Elem =
      if (this.set.isEmpty) Bot
      else if (this.set.forall(_.isCompletion)) AVT
      else if (this.set.forall(_.isPure)) AVF
      else bool
    def abruptCompletion: Elem =
      if (this.set contains AbruptT) Elem(AbruptT)
      else Bot

    /** absent helpers */
    def removeAbsent: Elem = this - AbsentT
    def isAbsent: Elem =
      if (set contains AbsentT) {
        if (set.size > 1) bool
        else AVT
      } else if (set.isEmpty) Bot
      else AVF

    /** refine receiver object */
    def refineThis(func: Func): Elem =
      if (func.isMethod) Elem(NameT(func.name.split("\\.").head))
      else this

    /** upcasting */
    def upcast: Elem = Elem(set.map(_.upcast)).norm

    /** normalize types */
    private def norm: Elem = {
      var set = this.set

      // TODO
      // // merge record
      // var record: Option[RecordT] = None
      // set.foreach {
      //   case r @ RecordT(props) =>
      //     set -= r
      //     record = record match {
      //       case Some(l) => Some(l ⊔ r)
      //       case None    => Some(r)
      //     }
      //   case _ =>
      // }
      // record.map(set += _)

      // remove redundant types
      set = set.filter(!_.strictAncestors.exists(this.set contains _))

      // merge singleton math
      if (set.count(_.isMath) > 1) {
        set = set.filter(!_.isMath)
        set += MathT
      }

      // merge aliases
      @tailrec
      def aux(pairs: List[(Type, Set[Type])]): Unit = pairs match {
        case (to, from) :: remain if from subsetOf set =>
          set --= from
          set += to
        case _ :: remain if set.size >= 2 => aux(remain)
        case _                            =>
      }
      aux(Type.typeAlias)
      Elem(set)
    }

    /** various assertions */
    def assert(f: Type => Boolean, msg: String = "") =
      if (!set.forall(f)) ??? // TODO warning
    def assertBool: Unit = assert(_.isBool)
    def assertStr: Unit = assert(_.isStr)
    def assertUndef: Unit = assert(_.isUndef)
    def assertNumeric: Unit = assert(_.isNumeric)
    def assertMath: Unit = assert(_.isMath)
    def assertNumber: Unit = assert(_.isBigInt)
    def assertBigInt: Unit = assert(_.isNumber)

    /** operation helpers */
    private def logicalOps(that: Elem) = {
      this.assertBool; that.assertBool; bool
    }
    private def mkNumericOps(
      mathCase: Elem,
      numberCase: Elem,
      bigIntCase: Elem,
    )(that: Elem) = {
      if (this.isBottom || that.isBottom) Bot
      else {
        this.assertNumeric
        that.assertNumeric
        var result: Elem = Bot
        if (this.set.exists(_.isMath)) { that.assertMath; result ⊔= mathCase }
        if (this.set.exists(_.isNumber)) {
          that.assertNumber; result ⊔= numberCase
        }
        if (this.set.exists(_.isBigInt)) {
          that.assertBigInt; result ⊔= bigIntCase
        }
        result
      }
    }
    private lazy val boolNumericOps = mkNumericOps(bool, bool, bool)
    private lazy val numericOps = mkNumericOps(math, num, bigint)
  }

}
