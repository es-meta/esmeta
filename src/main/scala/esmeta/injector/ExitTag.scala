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
    case ThrowValueTag(value: Value) => s"throw-value: $value"
    case ThrowErrorTag(errorName, msg) =>
      s"throw-error: ${errorName}${msg.map(msg => s"($msg)").getOrElse("")}"
  def equivalent(that: ExitTag): Boolean = (this, that) match
    case (_: ThrowValueTag, _: ThrowValueTag)               => true
    case (ThrowErrorTag(name1, _), ThrowErrorTag(name2, _)) => name1 == name2
    case _                                                  => this == that

object ExitTag:
  def apply(st: => State): ExitTag = try {
    st(GLOBAL_RESULT) match
      case Undef => NormalTag
      case comp @ Comp(ENUM_THROW, addr: DynamicAddr, _) =>
        st(addr)(Str("Prototype")) match
          case NamedAddr(errorNameRegex(errorName)) => ThrowErrorTag(errorName)
          case _                                    => ThrowValueTag(addr)
      case comp @ Comp(ENUM_THROW, value, _) => ThrowValueTag(value)
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

/** an error is thrown with a ECMAScript value */
case class ThrowValueTag(value: Value) extends ExitTag

/** an error is thrown with an ECMAScript error */
// TODO(@hyp3rflow): `msg` can be used with provenance; add this and extend stringifier
case class ThrowErrorTag(errorName: String, msg: Option[String] = None)
  extends ExitTag
