package esmeta.injector

import esmeta.error.*
import esmeta.state.*
import esmeta.util.BaseUtils.*
import java.util.concurrent.TimeoutException

/** exit status tag */
enum ExitTag extends InjectorElem {

  /* normal exit */
  case Normal

  /* timeout */
  case Timeout

  /* an error is thrown in specification */
  case SpecError(error: ESMetaError, cursor: Cursor)

  /** an error is thrown with an ECMAScript value */
  case ThrowValue(value: Value)

  /** check if the tag is normal */
  def isNormal: Boolean = this == Normal

  override def toString: String = this match
    case Normal                   => s"normal"
    case Timeout                  => s"timeout"
    case SpecError(error, cursor) => s"spec-error: $cursor"
    case ThrowValue(value)        => s"throw: $value"
}
object ExitTag:
  def apply(st: => State): ExitTag = try {
    st(GLOBAL_RESULT) match
      case Undef => Normal
      case addr: Addr =>
        st(addr) match
          case obj @ RecordObj("CompletionRecord", _) =>
            if (obj(Str("Type")) == ENUM_THROW) ThrowValue(obj(Str("Value")))
            else error(s"unexpected exit status: $addr")
          case _ => error(s"unexpected exit status: $addr")
      case v => error(s"unexpected exit status: $v")
  } catch {
    case _: TimeoutException   => Timeout
    case e: InterpreterErrorAt => SpecError(e.error, e.cursor)
  }

  /** error name regex pattern */
  lazy val errorNameRegex = "INTRINSICS.([A-Z][a-z]+Error).prototype".r
