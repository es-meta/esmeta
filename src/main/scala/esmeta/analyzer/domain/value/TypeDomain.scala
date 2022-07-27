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
  def apply(ast: Ast): Elem = ???
  def apply(num: Number): Elem = ???
  def apply(num: Double): Elem = ???
  def apply(bigint: BigInt): Elem = ???
  def apply(str: String): Elem = ???
  def apply(bool: Boolean): Elem = ???
  def apply(d: BigDecimal): Elem = ???
  lazy val codeunit: Elem = ???
  lazy val math: Elem = ???
  lazy val num: Elem = ???
  lazy val bigint: Elem = ???
  lazy val str: Elem = ???
  lazy val bool: Elem = ???
  lazy val undef: Elem = ???
  lazy val nullv: Elem = ???
  lazy val absent: Elem = ???
  def apply(value: Value): Elem = ???
  def apply(value: AValue): Elem = ???

  /** constructors */
  def apply(
    comp: AbsComp = AbsComp.Bot,
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    loc: AbsLoc = AbsLoc.Bot,
    ast: AbsAst = AbsAst.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeunit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simple: AbsSimple = AbsSimple.Bot,
    num: AbsNum = AbsNum.Bot,
    bigint: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = ???
  def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem = ???

  /** appender */
  given rule: Rule[Elem] = ???

  /** elements */
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
      apply(bool = (this.set.toList, that.set.toList) match
        case (Nil, Nil)                           => AbsBool.Bot
        case (List(l: SingleT), List(r: SingleT)) => AbsBool(Bool(l == r))
        case _ if (this ⊓ that).isBottom          => AF
        case _                                    => AB,
      )
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
    // TODO AbsValue(bool = !operand.bool)
    def unary_~ : Elem = ???
    def abs: Elem = ???
    def floor: Elem = ???

    /** type operations */
    def typeOf(st: AbsState): Elem = ???
    def typeCheck(tname: String, st: AbsState): Elem = ???

    /** helper functions for abstract transfer */
    def convert(cop: COp, radix: Elem): Elem = ???
    def sourceText: Elem = ???
    def parse(rule: Elem): Elem = ???

    /** prune abstract values */
    def pruneType(r: Elem, positive: Boolean): Elem = ???

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
    def isCompletion: AbsBool = ???
    def abruptCompletion: Elem = ???

    /** absent helpers */
    def removeAbsent: Elem = this - AbsentT
    def isAbsent: AbsBool = ???

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
