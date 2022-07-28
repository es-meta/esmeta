package esmeta.analyzer.domain

import esmeta.interp.*
import esmeta.ir.COp
import esmeta.js.Ast
import esmeta.util.Appender
import esmeta.util.Appender.*
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
  def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem = ???

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    given tyRule: Rule[Type] = (app, ty) => app >> ty.toString
    given setRule: Rule[Iterable[Type]] = iterableRule("(", " | ", ")")
    elem.set.size match
      case 0 => app >> "⊥"
      case 1 => app >> elem.set.head
      case _ => app >> elem.set

  /** elements */
  object Elem:
    def apply(tys: Type*): Elem = apply(tys)
    def apply(tys: Iterable[Type]): Elem = Elem(tys.toSet).norm
  case class Elem(set: Set[Type]) extends ValueElemTrait {

    /** getters */
    def loc: AbsLoc = ???
    def getKeyValue: Elem = ??? // XXX not used
    def getDescValue: Elem = ???
    def getClo: List[AClo] = ???
    def getCont: List[ACont] = ???

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
    def <(that: Elem): Elem = ???

    /** logical operations */
    def &&(that: Elem): Elem = ???
    def ||(that: Elem): Elem = ???
    def ^^(that: Elem): Elem = ???

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
    def typeOf(st: AbsState): Elem = Elem(for {
      t <- set
      y <- t.typeNameSet
    } yield StrSingleT(y))
    def typeCheck(tname: String, st: AbsState): Elem = ???

    /** helper functions for abstract transfer */
    def convert(cop: COp, radix: Elem): Elem = ???
    def sourceText: Elem = ???
    def parse(rule: Elem): Elem = ???
    def duplicated(st: AbsState): Elem = ???

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
            case _           => ???
          if (positive) this ⊓ Elem(ty) else this - ty
        case _ => ???

    /** singleton */
    def getSingle: Flat[AValue] = this.set.headOption match
      case None                                    => FlatBot
      case Some(ty: SingleT) if this.set.size == 1 => FlatElem(AValue.from(ty))
      case _                                       => FlatTop

    /** get reachable locations */
    def reachableLocs: Set[Loc] = Set() // XXX not used

    /** completion helpers */
    def wrapCompletion: Elem = Elem(set.map(_.wrapCompletion)).norm
    def unwrapCompletion: Elem = ???
    def isCompletion: Elem = ???
    def abruptCompletion: Elem = ???

    /** absent helpers */
    def removeAbsent: Elem = this - AbsentT
    def isAbsent: Elem = ???

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

  }

}
