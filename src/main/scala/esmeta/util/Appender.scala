package esmeta.util

import esmeta.LINE_SEP

/** a wrapper of java.lang.StringBuilder in a typeclass style
  *
  * @param tab
  *   a string for tab
  */
class Appender(tab: String = "  ") {
  import Appender.*
  private val sb: StringBuilder = new StringBuilder
  override def toString: String = sb.toString

  /** current indentation * */
  var indent = 0
  private def indentStr: String = tab * indent

  /** append an appendable */
  def >>[T: Appendable](x: T)(using a: Appendable[T]): Appender = a(this, x)

  /** append an appendable with a newline and an indentation */
  def :>[T: Appendable](x: T)(using a: Appendable[T]): Appender =
    a(this >> LINE_SEP >> indentStr, x)

  /** wrap without brackets and one-level higher indentation */
  def wrap(f: => Unit): Appender = wrap("", f, "")

  /** wrap with open/close brackets and one-level higher indentation */
  def wrap(open: String, f: => Unit, close: String): Appender =
    this >> open
    indent += 1; f; indent -= 1
    this :> close

  /** wrap iterable with detail option */
  def wrapIterable[T](iter: Iterable[T], detail: Boolean = true)(using
    tApp: Appendable[T],
  ): Appender =
    if iter.isEmpty then this >> "{}"
    else if !detail then this >> "{ ... }"
    else this.wrap { for (elem <- iter) this :> elem }
}

/** helper for appender */
object Appender {

  /** appendable * */
  type Appendable[T] = (Appender, T) => Appender

  /** iterable with separator */
  def iterableApp[T](
    left: String = "",
    sep: String = "",
    right: String = "",
  )(using tApp: Appendable[T]): Appendable[Iterable[T]] = (app, iter) =>
    app >> left
    if (!iter.isEmpty) {
      app >> iter.head
      for (x <- iter.tail) app >> sep >> x
    }
    app >> right

  /** arrows for pairs */
  def arrowApp[T, U](using
    tApp: Appendable[T],
    uApp: Appendable[U],
  ): Appendable[(T, U)] = (app, pair) =>
    val (t, u) = pair
    app >> t >> " -> " >> u

  /** map appender */
  def mapApp[K, V](using
    kApp: Appendable[K],
    vApp: Appendable[V],
  ): Appendable[Map[K, V]] = (app, map) =>
    given Appendable[(K, V)] = arrowApp
    if (map.size == 0) app >> "{}"
    else app.wrap(for (pair <- map) app :> pair)

  /** sorted map appender */
  def sortedMapApp[K, V](using
    kOrd: Ordering[K],
    kApp: Appendable[K],
    vApp: Appendable[V],
  ): Appendable[Map[K, V]] = (app, map) =>
    given Appendable[(K, V)] = arrowApp
    if (map.size == 0) app >> "{}"
    else app.wrap(for (pair <- map.toList.sortBy(_._1)) app :> pair)

  // basic values
  given Appendable[String] = (app, str) => { app.sb ++= str; app }
  given Appendable[Int] = _ >> _.toString
  given Appendable[Long] = _ >> _.toString
  given Appendable[Boolean] = _ >> _.toString
}
