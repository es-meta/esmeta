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

  /** append an Rule */
  def >>[T: Rule](x: T)(using a: Rule[T]): Appender = a(this, x)

  /** append an Rule with a newline and an indentation */
  def :>[T: Rule](x: T)(using a: Rule[T]): Appender =
    a(this >> LINE_SEP >> indentStr, x)

  /** wrap without brackets and one-level higher indentation */
  def wrap(f: => Unit): Appender = wrap("{", "}")(f)

  /** wrap with open/close brackets and one-level higher indentation */
  def wrap(open: String, close: String)(f: => Unit): Appender =
    this >> open
    indent += 1; f; indent -= 1
    if (close != "") this :> close else this

  /** wrap iterable with detail option */
  def wrapIterable[T](iter: Iterable[T])(using tRule: Rule[T]): Appender =
    wrapIterable()(iter)
  def wrapIterable[T](
    open: String = "{",
    close: String = "}",
    detail: Boolean = true,
  )(iter: Iterable[T])(using tRule: Rule[T]): Appender =
    if (iter.isEmpty) this >> open >> close
    else if (!detail) this >> open >> " ... " >> close
    else this.wrap(open, close) { for (elem <- iter) this :> elem >> "," }
}

/** helper for appender */
object Appender {

  /** Rule * */
  type Rule[T] = (Appender, T) => Appender

  /** subtype rule */
  given subtypeRule[S, T <: S](using sRule: Rule[S]): Rule[T] =
    (app, tElem) => sRule(app, tElem)

  /** optional with default string */
  def optionRule[T](
    defaultStr: String,
  )(using tRule: Rule[T]): Rule[Option[T]] = (app, opt) =>
    opt match {
      case Some(x) => app >> x
      case None    => app >> defaultStr
    }

  /** iterable with separator */
  def iterableRule[T](
    left: String = "",
    sep: String = "",
    right: String = "",
  )(using tRule: Rule[T]): Rule[Iterable[T]] = (app, iter) =>
    app >> left
    if (!iter.isEmpty) {
      app >> iter.head
      for (x <- iter.tail) app >> sep >> x
    }
    app >> right

  /** arrows for pairs */
  given arrowRule[T, U](using
    tRule: Rule[T],
    uRule: Rule[U],
  ): Rule[(T, U)] = (app, pair) =>
    val (t, u) = pair
    app >> t >> " -> " >> u

  /** map appender */
  given mapRule[K, V](using
    kRule: Rule[K],
    vRule: Rule[V],
  ): Rule[Map[K, V]] = (app, map) =>
    given Rule[(K, V)] = arrowRule
    if (map.size == 0) app >> "{}"
    else app.wrap(for (pair <- map) app :> pair)

  /** sorted map appender */
  def sortedMapRule[K, V](using
    kOrd: Ordering[K],
    kRule: Rule[K],
    vRule: Rule[V],
  ): Rule[Map[K, V]] = (app, map) =>
    given Rule[(K, V)] = arrowRule
    if (map.size == 0) app >> "{}"
    else app.wrap(for (pair <- map.toList.sortBy(_._1)) app :> pair)

  // basic values
  given stringRule: Rule[String] = (app, str) => { app.sb ++= str; app }
  given charRule: Rule[Char] = (app, ch) => { app.sb += ch; app }
  given intRule: Rule[Int] = _ >> _.toString
  given bigDecimalRule: Rule[BigDecimal] = _ >> _.toString
  given bigintRule: Rule[BigInt] = _ >> _.toString
  given doubleRule: Rule[Double] = _ >> _.toString
  given longRule: Rule[Long] = _ >> _.toString
  given booleanRule: Rule[Boolean] = _ >> _.toString
}
