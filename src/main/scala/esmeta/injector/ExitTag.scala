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

  /** an ECMAScript error object is thrown */
  case ThrowError(name: String)

  /** check if the tag is normal */
  def isNormal: Boolean = this == Normal

  /** compare exit behavior while abstracting non-error thrown values */
  def equivalent(that: ExitTag): Boolean = (this, that) match
    case (ThrowValue(_), ThrowValue(_)) => true
    case (ThrowError(x), ThrowError(y)) => x == y
    case _                              => this == that
}
object ExitTag {
  private val errorNamePattern = raw"INTRINSICS\.([A-Za-z]*Error)\.prototype".r

  def apply(st: => State): ExitTag = try {
    def errorWith(v: Value): Nothing =
      raise(s"unexpected exit status: ${st.getString(v)}")
    st(GLOBAL_RESULT) match
      case Undef => Normal
      case addr: Addr =>
        st(addr) match
          case ListObj(values @ Vector(errorAddr: Addr)) =>
            st.get(errorAddr, Str("Prototype")).toOption match
              case Some(NamedAddr(errorNamePattern(name))) => ThrowError(name)
              case _                                       => ThrowValue(values)
          case ListObj(values) => ThrowValue(values)
          case _               => errorWith(addr)
      case v => errorWith(v)
  } catch {
    case _: TimeoutException   => Timeout
    case e: InterpreterErrorAt => SpecError(e.error, e.cursor)
  }
}
