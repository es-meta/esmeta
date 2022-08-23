package esmeta.ai.domain.simpleValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** basic domain for simple values */
case class BasicDomain(config: Config) extends simpleValue.Domain {
  import config.*

  /** elements */
  case class Elem(
    number: AbsNumber = AbsNumber.Bot,
    bigInt: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  )

  /** top element */
  val Top = Elem(
    number = AbsNumber.Top,
    bigInt = AbsBigInt.Top,
    str = AbsStr.Top,
    bool = AbsBool.Top,
    undef = AbsUndef.Top,
    nullv = AbsNull.Top,
    absent = AbsAbsent.Top,
  )

  /** bottom element */
  val Bot = Elem()

  /** abstraction functions */
  def alpha(xs: Iterable[SimpleValue]): Elem = Elem(
    AbsNumber(xs.collect { case x: Number => x }),
    AbsBigInt(xs.collect { case x: BigInt => x }),
    AbsStr(xs.collect { case x: Str => x }),
    AbsBool(xs.collect { case x: Bool => x }),
    AbsUndef(xs.collect { case x: Undef => x }),
    AbsNull(xs.collect { case x: Null => x }),
    AbsAbsent(xs.collect { case x: Absent => x }),
  )

  /** predefined number top */
  val numberTop: Elem = Bot.copy(number = AbsNumber.Top)
  val bigIntTop: Elem = Bot.copy(bigInt = AbsBigInt.Top)
  val strTop: Elem = Bot.copy(str = AbsStr.Top)
  val boolTop: Elem = Bot.copy(bool = AbsBool.Top)
  val undefTop: Elem = Bot.copy(undef = AbsUndef.Top)
  val nullTop: Elem = Bot.copy(nullv = AbsNull.Top)
  val absentTop: Elem = Bot.copy(absent = AbsAbsent.Top)
  def apply(simple: SimpleValue): Elem = simple match
    case number: Number => Elem(number = AbsNumber(number))
    case bigInt: BigInt => Elem(bigInt = AbsBigInt(bigInt))
    case str: Str       => Elem(str = AbsStr(str))
    case bool: Bool     => Elem(bool = AbsBool(bool))
    case Undef          => undefTop
    case Null           => nullTop
    case Absent         => absentTop

  /** constructors */
  def apply(
    number: AbsNumber = AbsNumber.Bot,
    bigInt: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = Elem(number, bigInt, str, bool, undef, nullv, absent)

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple] = Some(
    (
      elem.number,
      elem.bigInt,
      elem.str,
      elem.bool,
      elem.undef,
      elem.nullv,
      elem.absent,
    ),
  )

  /** appender */
  given rule: Rule[Elem] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      val Elem(number, bigInt, str, bool, undef, nullv, absent) = elem
      var strs = Vector[String]()
      if (!number.isBottom) strs :+= number.toString
      if (!bigInt.isBottom) strs :+= bigInt.toString
      if (!str.isBottom) strs :+= str.toString
      if (!bool.isBottom) strs :+= bool.toString
      if (!undef.isBottom) strs :+= undef.toString
      if (!nullv.isBottom) strs :+= nullv.toString
      if (!absent.isBottom) strs :+= absent.toString
      app >> strs.mkString(", ")
    }
  }

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = (
      elem.number ⊑ that.number &&
        elem.bigInt ⊑ that.bigInt &&
        elem.str ⊑ that.str &&
        elem.bool ⊑ that.bool &&
        elem.undef ⊑ that.undef &&
        elem.nullv ⊑ that.nullv &&
        elem.absent ⊑ that.absent
    )

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(
      elem.number ⊔ that.number,
      elem.bigInt ⊔ that.bigInt,
      elem.str ⊔ that.str,
      elem.bool ⊔ that.bool,
      elem.undef ⊔ that.undef,
      elem.nullv ⊔ that.nullv,
      elem.absent ⊔ that.absent,
    )

    /** meet operator */
    def ⊓(that: Elem): Elem = Elem(
      elem.number ⊓ that.number,
      elem.bigInt ⊓ that.bigInt,
      elem.str ⊓ that.str,
      elem.bool ⊓ that.bool,
      elem.undef ⊓ that.undef,
      elem.nullv ⊓ that.nullv,
      elem.absent ⊓ that.absent,
    )

    /** minus operator */
    def -(that: Elem): Elem = Elem(
      elem.number - that.number,
      elem.bigInt - that.bigInt,
      elem.str - that.str,
      elem.bool - that.bool,
      elem.undef - that.undef,
      elem.nullv - that.nullv,
      elem.absent - that.absent,
    )

    /** concretization function */
    override def gamma: BSet[SimpleValue] =
      elem.number.gamma ⊔
      elem.bigInt.gamma ⊔
      elem.str.gamma ⊔
      elem.bool.gamma ⊔
      elem.undef.gamma ⊔
      elem.nullv.gamma ⊔
      elem.absent.gamma

    /** get single value */
    override def getSingle: Flat[SimpleValue] =
      elem.number.getSingle ⊔
      elem.bigInt.getSingle ⊔
      elem.str.getSingle ⊔
      elem.bool.getSingle ⊔
      elem.undef.getSingle ⊔
      elem.nullv.getSingle ⊔
      elem.absent.getSingle
  }
}
