package esmeta.analyzer.domain

import esmeta.state.*
import esmeta.util.Appender
import esmeta.util.Appender.*

/** basic abstract domain for simple value */
object BasicSimpleValueDomain extends Domain {
  val Bot = Elem(
    num = AbsNum.Bot,
    bigint = AbsBigInt.Bot,
    str = AbsStr.Bot,
    bool = AbsBool.Bot,
    undef = AbsUndef.Bot,
    nullv = AbsNull.Bot,
    absent = AbsAbsent.Bot,
  )
  lazy val Top = Elem(
    num = AbsNum.Top,
    bigint = AbsBigInt.Top,
    str = AbsStr.Top,
    bool = AbsBool.Top,
    undef = AbsUndef.Top,
    nullv = AbsNull.Top,
    absent = AbsAbsent.Top,
  )

  // abstraction functions
  def apply(num: Number): Elem = Bot.copy(num = AbsNum(num))
  def apply(num: Double): Elem = Bot.copy(num = AbsNum(Number(num)))
  def apply(bigint: BigInt): Elem = Bot.copy(bigint = AbsBigInt(bigint))
  def apply(bigint: scala.math.BigInt): Elem =
    Bot.copy(bigint = AbsBigInt(BigInt(bigint)))
  def apply(str: String): Elem = Bot.copy(str = AbsStr(Str(str)))
  def apply(bool: Boolean): Elem = Bot.copy(bool = AbsBool(Bool(bool)))
  lazy val num: Elem = Bot.copy(num = AbsNum.Top)
  lazy val bigint: Elem = Bot.copy(bigint = AbsBigInt.Top)
  lazy val str: Elem = Bot.copy(str = AbsStr.Top)
  lazy val bool: Elem = Bot.copy(bool = AbsBool.Top)
  lazy val undef: Elem = Bot.copy(undef = AbsUndef.Top)
  lazy val nullv: Elem = Bot.copy(nullv = AbsNull.Top)
  lazy val absent: Elem = Bot.copy(absent = AbsAbsent.Top)
  def apply(simple: ASimple): Elem = simple.value match
    case num: Number    => this(num)
    case BigInt(bigint) => this(bigint)
    case Str(str)       => this(str)
    case Bool(bool)     => this(bool)
    case Undef          => this.undef
    case Null           => this.nullv
    case Absent         => this.absent

  // constructors
  def apply(
    num: AbsNum = AbsNum.Bot,
    bigint: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = Elem(num, bigint, str, bool, undef, nullv, absent)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.num,
      elem.bigint,
      elem.str,
      elem.bool,
      elem.undef,
      elem.nullv,
      elem.absent,
    ),
  )

  // appender
  given rule: Rule[Elem] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      val Elem(num, bigint, str, bool, undef, nullv, absent) = elem
      var strs = Vector[String]()
      if (!num.isBottom) strs :+= num.toString
      if (!bigint.isBottom) strs :+= bigint.toString
      if (!str.isBottom) strs :+= str.toString
      if (!bool.isBottom) strs :+= bool.toString
      if (!undef.isBottom) strs :+= undef.toString
      if (!nullv.isBottom) strs :+= nullv.toString
      if (!absent.isBottom) strs :+= absent.toString
      app >> strs.mkString(", ")
    }
  }

  // elements
  case class Elem(
    num: AbsNum,
    bigint: AbsBigInt,
    str: AbsStr,
    bool: AbsBool,
    undef: AbsUndef,
    nullv: AbsNull,
    absent: AbsAbsent,
  ) extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (
      this.num ⊑ that.num &&
        this.bigint ⊑ that.bigint &&
        this.str ⊑ that.str &&
        this.bool ⊑ that.bool &&
        this.undef ⊑ that.undef &&
        this.nullv ⊑ that.nullv &&
        this.absent ⊑ that.absent
    )

    // join operator
    def ⊔(that: Elem): Elem = Elem(
      this.num ⊔ that.num,
      this.bigint ⊔ that.bigint,
      this.str ⊔ that.str,
      this.bool ⊔ that.bool,
      this.undef ⊔ that.undef,
      this.nullv ⊔ that.nullv,
      this.absent ⊔ that.absent,
    )

    // meet operator
    def ⊓(that: Elem): Elem = Elem(
      this.num ⊓ that.num,
      this.bigint ⊓ that.bigint,
      this.str ⊓ that.str,
      this.bool ⊓ that.bool,
      this.undef ⊓ that.undef,
      this.nullv ⊓ that.nullv,
      this.absent ⊓ that.absent,
    )

    // minus operator
    def -(that: Elem): Elem = Elem(
      this.num - that.num,
      this.bigint - that.bigint,
      this.str - that.str,
      this.bool - that.bool,
      this.undef - that.undef,
      this.nullv - that.nullv,
      this.absent - that.absent,
    )

    // get single value
    def getSingle: Flat[ASimple] = (
      this.num.getSingle ⊔
        this.bigint.getSingle ⊔
        this.str.getSingle ⊔
        this.bool.getSingle ⊔
        this.undef.getSingle ⊔
        this.nullv.getSingle ⊔
        this.absent.getSingle
    ).map(ASimple.apply)

    // remove absent values
    def removeAbsent: Elem = copy(absent = AbsAbsent.Bot)
  }
}
