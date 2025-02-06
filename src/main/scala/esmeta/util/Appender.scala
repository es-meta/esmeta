package esmeta.util

import esmeta.LINE_SEP
import esmeta.util.BaseUtils.*

/** a wrapper of java.lang.StringBuilder in a typeclass style
  *
  * @param tab
  *   a string for tab
  */
class Appender(tab: String = "  ") {
  import Appender.*
  private val sb: StringBuilder = StringBuilder()
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
    sep: String = ",",
    close: String = "}",
    detail: Boolean = true,
  )(iter: Iterable[T])(using tRule: Rule[T]): Appender =
    if (iter.isEmpty) this >> open >> close
    else if (!detail) this >> open >> " ... " >> close
    else this.wrap(open, close) { for (elem <- iter) this :> elem >> sep }
}

/** helper for appender */
object Appender {

  /** appender rule */
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
  ): Rule[(T, U)] = arrowRule()
  def arrowRule[T, U](
    sep: String = " -> ",
  )(using
    tRule: Rule[T],
    uRule: Rule[U],
  ): Rule[(T, U)] = (app, pair) =>
    val (t, u) = pair
    app >> t >> sep >> u

  /** YAML appender */
  given yamlRule: Rule[Yaml] = yamlWithIndentRule(false)

  /** YAML appender with indent */
  def yamlWithIndentRule(wrap: Boolean): Rule[Yaml] = (app, yaml) =>
    given Rule[Yaml] = yamlWithIndentRule(true)
    given iterableRule[T](using Rule[T]): Rule[Iterable[T]] = (app, iter) =>
      if (wrap) app.wrap("", "")(iter.map(app :> _))
      else
        iter.splitAt(1) match
          case (hd, tl) if !hd.isEmpty =>
            app >> hd.head
            tl.map(app :> _)
          case _ =>
      app
    yaml match
      case YMap(items) =>
        given Rule[(String, Yaml)] = { case (a, (k, v)) => a >> k >> ":" >> v }
        app >> items
      case YList(items) =>
        given Rule[Yaml] = {
          case (a, v) =>
            given Rule[Yaml] = yamlWithIndentRule(false)
            a >> "- " >> v
        }
        app >> items
      case YString(value) =>
        if (wrap) app >> " " >> value
        else app >> value
    app

  /** map appender */
  given mapRule[K, V](using
    kOrd: Ordering[K],
    kRule: Rule[K],
    vRule: Rule[V],
  ): Rule[Iterable[(K, V)]] = sortedMapRule()

  /** sorted map appender */
  def sortedMapRule[K, V](
    left: String = "{",
    right: String = "}",
    sep: String = " -> ",
  )(using
    kOrd: Ordering[K],
    kRule: Rule[K],
    vRule: Rule[V],
  ): Rule[Iterable[(K, V)]] = (app, map) =>
    given Rule[(K, V)] = arrowRule(sep)
    if (map.size == 0) app >> left >> right
    else app.wrap(for (pair <- map.toList.sortBy(_._1)) app :> pair)

  // basic values
  given stringRule: Rule[String] = (app, str) => { app.sb ++= str; app }
  given charRule: Rule[Char] = (app, ch) => { app.sb += ch; app }
  given intRule: Rule[Int] = _ >> _.toString
  given bigDecimalRule: Rule[BigDecimal] = _ >> _.toString
  given bigIntRule: Rule[BigInt] = _ >> _.toString
  given doubleRule: Rule[Double] = _ >> _.toString
  given longRule: Rule[Long] = _ >> _.toString
  given booleanRule: Rule[Boolean] = _ >> _.toString
}

/** appendable */
trait Appendable[T <: Appendable[T]] { self: T =>

  /** appender */
  given rule: Appender.Rule[T]

  /** conversion to string */
  override def toString: String = stringify(this)
}
