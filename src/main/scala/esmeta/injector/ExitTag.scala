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
  case ThrowValue(values: Vector[Value])

  /** check if the tag is normal */
  def isNormal: Boolean = this == Normal
}
object ExitTag {
  def apply(st: => State): ExitTag = try {
    def errorWith(v: Value): Nothing =
      raise(s"unexpected exit status: ${st.getString(v)}")
    st(GLOBAL_RESULT) match
      case Undef => Normal
      case addr: Addr =>
        st(addr) match
          case obj @ ListObj(values) => ThrowValue(values)
          case _                     => errorWith(addr)
      case v => errorWith(v)
  } catch {
    case _: TimeoutException   => Timeout
    case e: InterpreterErrorAt => SpecError(e.error, e.cursor)
  }
}
