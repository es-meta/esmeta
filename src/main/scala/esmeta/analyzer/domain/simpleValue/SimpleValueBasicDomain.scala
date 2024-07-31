package esmeta.analyzer.domain.simpleValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

trait SimpleValueBasicDomainDecl { self: Self =>

  /** basic domain for simple values */
  object SimpleValueBasicDomain extends SimpleValueDomain {

    /** elements */
    case class Elem(
      number: AbsNumber = AbsNumber.Bot,
      bigInt: AbsBigInt = AbsBigInt.Bot,
      str: AbsStr = AbsStr.Bot,
      bool: AbsBool = AbsBool.Bot,
      undef: AbsUndef = AbsUndef.Bot,
      nullv: AbsNull = AbsNull.Bot,
    ) extends Appendable

    /** top element */
    val Top = Elem(
      number = AbsNumber.Top,
      bigInt = AbsBigInt.Top,
      str = AbsStr.Top,
      bool = AbsBool.Top,
      undef = AbsUndef.Top,
      nullv = AbsNull.Top,
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
    )

    /** predefined top values */
    val numberTop: Elem = Bot.copy(number = AbsNumber.Top)
    val bigIntTop: Elem = Bot.copy(bigInt = AbsBigInt.Top)
    val strTop: Elem = Bot.copy(str = AbsStr.Top)
    val boolTop: Elem = Bot.copy(bool = AbsBool.Top)
    val undefTop: Elem = Bot.copy(undef = AbsUndef.Top)
    val nullTop: Elem = Bot.copy(nullv = AbsNull.Top)

    /** constructors */
    def apply(
      number: AbsNumber = AbsNumber.Bot,
      bigInt: AbsBigInt = AbsBigInt.Bot,
      str: AbsStr = AbsStr.Bot,
      bool: AbsBool = AbsBool.Bot,
      undef: AbsUndef = AbsUndef.Bot,
      nullv: AbsNull = AbsNull.Bot,
    ): Elem = Elem(number, bigInt, str, bool, undef, nullv)

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = Some(
      (
        elem.number,
        elem.bigInt,
        elem.str,
        elem.bool,
        elem.undef,
        elem.nullv,
      ),
    )

    /** appender */
    given rule: Rule[Elem] = (app, elem) => {
      if (elem.isBottom) app >> "⊥"
      else {
        val Elem(number, bigInt, str, bool, undef, nullv) = elem
        var strs = Vector[String]()
        if (!number.isBottom) strs :+= number.toString
        if (!bigInt.isBottom) strs :+= bigInt.toString
        if (!str.isBottom) strs :+= str.toString
        if (!bool.isBottom) strs :+= bool.toString
        if (!undef.isBottom) strs :+= undef.toString
        if (!nullv.isBottom) strs :+= nullv.toString
        app >> strs.mkString(", ")
      }
    }

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.number ⊑ that.number &&
        elem.bigInt ⊑ that.bigInt &&
        elem.str ⊑ that.str &&
        elem.bool ⊑ that.bool &&
        elem.undef ⊑ that.undef &&
        elem.nullv ⊑ that.nullv

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.number ⊔ that.number,
        elem.bigInt ⊔ that.bigInt,
        elem.str ⊔ that.str,
        elem.bool ⊔ that.bool,
        elem.undef ⊔ that.undef,
        elem.nullv ⊔ that.nullv,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.number ⊓ that.number,
        elem.bigInt ⊓ that.bigInt,
        elem.str ⊓ that.str,
        elem.bool ⊓ that.bool,
        elem.undef ⊓ that.undef,
        elem.nullv ⊓ that.nullv,
      )

      /** minus operator */
      override def --(that: Elem): Elem = Elem(
        elem.number -- that.number,
        elem.bigInt -- that.bigInt,
        elem.str -- that.str,
        elem.bool -- that.bool,
        elem.undef -- that.undef,
        elem.nullv -- that.nullv,
      )

      /** concretization function */
      override def gamma: BSet[SimpleValue] =
        elem.number.gamma ⊔
        elem.bigInt.gamma ⊔
        elem.str.gamma ⊔
        elem.bool.gamma ⊔
        elem.undef.gamma ⊔
        elem.nullv.gamma

      /** get single value */
      override def getSingle: Flat[SimpleValue] =
        elem.number.getSingle ⊔
        elem.bigInt.getSingle ⊔
        elem.str.getSingle ⊔
        elem.bool.getSingle ⊔
        elem.undef.getSingle ⊔
        elem.nullv.getSingle

      /** getters */
      def number: AbsNumber = elem.number
      def bigInt: AbsBigInt = elem.bigInt
      def str: AbsStr = elem.str
      def bool: AbsBool = elem.bool
      def undef: AbsUndef = elem.undef
      def nullv: AbsNull = elem.nullv
    }
  }
}
