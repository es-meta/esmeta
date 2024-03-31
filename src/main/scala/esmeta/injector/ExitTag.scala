package esmeta.injector

import esmeta.error.*
import esmeta.state.*
import esmeta.util.BaseUtils.*
import java.util.concurrent.TimeoutException

/** exit status tag */
trait ExitTag:
  override def toString: String = this match
    case NormalTag                   => s"normal"
    case TimeoutTag                  => s"timeout"
    case SpecErrorTag(error, cursor) => s"spec-error: $cursor"
    case ThrowTag(items, msg) =>
      s"throw: ${items.mkString(", ")}${msg.fold("")(m => s"($m)")}"
  def equivalent(that: ExitTag): Boolean = (this, that) match
    case (ThrowTag(list1, _), ThrowTag(list2, _)) =>
      (list1 zip list2).forall(_ equivalent _)
    case _ => this == that

object ExitTag:
  def apply(st: => State): ExitTag = try {
    def getThrowItem(value: Value) = value match
      case addr: DynamicAddr =>
        st(addr)(Str("Ptototype")) match
          case NamedAddr(errorNameRegex(errorName)) =>
            ThrowNativeError(errorName)
          case _ => ThrowValue(addr)
      case _ => ThrowValue(value)
    st(GLOBAL_RESULT) match
      case Undef => NormalTag
      case addr: Addr =>
        st(addr) match
          case ListObj(list) => ThrowTag(list.map(getThrowItem))
          case _             => error(s"unexpected exit status: $addr")
      case v => error(s"unexpected exit status: $v")
  } catch {
    case _: TimeoutException   => TimeoutTag
    case e: InterpreterErrorAt => SpecErrorTag(e.error, e.cursor)
  }

  /** error name regex pattern */
  lazy val errorNameRegex = "INTRINSICS.([A-Z][a-z]+Error).prototype".r

/** normal exit */
case object NormalTag extends ExitTag

/** timeout */
case object TimeoutTag extends ExitTag

/** an error is thrown in specification */
case class SpecErrorTag(error: ESMetaError, cursor: Cursor) extends ExitTag

/** an error is thrown with ECMAScript error or ECMAScript value */
// TODO(@hyp3rflow): `msg` can be used with provenance (maybe); add this and extend stringifier
case class ThrowTag(items: Vector[ThrowItem], msg: Option[String] = None)
  extends ExitTag

object ThrowTag:
  def apply(errorName: String): ThrowTag =
    ThrowTag(Vector(ThrowNativeError(errorName)))
  def apply(value: Value): ThrowTag =
    ThrowTag(Vector(ThrowValue(value)))

trait ThrowItem:
  def equivalent(that: ThrowItem): Boolean =
    (this, that) match
      case (ThrowNativeError(error1), ThrowNativeError(error2)) =>
        error1 == error2
      case (ThrowValue(_), ThrowValue(_)) => true
      case _                              => false

/** an ECMAScript error */
case class ThrowNativeError(errorName: String) extends ThrowItem

/** an ECMAScript value */
case class ThrowValue(value: Value) extends ThrowItem
