package esmeta.ai.domain.simpleValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** abstract domain for simple values */
trait Domain
  extends domain.Domain[SimpleValue]
  with Prunable[SimpleValue]
  with Meetable[SimpleValue] {

  /** domain configuration */
  val config: Config

  /** load domain configuration */
  import config.*

  /** element constructors with Scala values */
  def apply(num: Double): Elem = alpha(Number(num))
  def apply(str: String): Elem = alpha(Str(str))
  def apply(bigInt: scala.math.BigInt): Elem = alpha(BigInt(bigInt))
  def apply(bool: Boolean): Elem = alpha(Bool(bool))

  /** predefined number top */
  val numberTop: Elem
  val bigIntTop: Elem
  val strTop: Elem
  val boolTop: Elem
  val undefTop: Elem
  val nullTop: Elem
  val absentTop: Elem

  /** constructors */
  def apply(
    num: AbsNumber = AbsNumber.Bot,
    bigInt: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem

  /** raw tuple of each simple value type */
  type RawTuple = (
    AbsNumber,
    AbsBigInt,
    AbsStr,
    AbsBool,
    AbsUndef,
    AbsNull,
    AbsAbsent,
  )

  /** extractors */
  def unapply(elem: Elem): Option[RawTuple]

  /** simple value element interfaces */
  extension (elem: Elem) {

    /** remove absent values */
    def removeAbsent: Elem = elem - absentTop
  }
}
