package esmeta.util

import Math.{log, round}
import java.text.SimpleDateFormat
import java.util.Date
import esmeta.*
import esmeta.error.*
import scala.Console.*
import scala.collection.mutable
import scala.util.Random

/** base utilities */
object BaseUtils {

  /** create a new function with a cache */
  def cached[A, B](f: A => B): A => B = {
    val cache = mutable.Map.empty[A, B]
    arg =>
      cache.getOrElse(
        arg, {
          val res = f(arg)
          cache.update(arg, res)
          res
        },
      )
  }

  /** throw a simple error */
  def error(msg: String): Nothing = throw ESMetaError(msg)

  /** show a warning message */
  def warn(msg: String): Unit = Console.err.println(s"[WARNING] $msg")

  /** get duration time */
  def time[T](f: => T): (Long, T) = {
    val start = System.currentTimeMillis
    val result = f
    val end = System.currentTimeMillis
    (end - start, result)
  }

  /** show duration time with loading message */
  def time[T](msg: String, f: => T): (Long, T) = {
    lazy val f0 = f
    print(s"$msg...")
    val (interval, res) = time(f0)
    println(f" ($interval%,d ms)")
    (interval, res)
  }

  /** show colored message */
  def setColor(color: String): Any => String =
    if (color == "") x => x.toString else x => color + x.toString + RESET
  def printColor(color: String): Any => Unit = x => print(setColor(color)(x))
  def printlnColor(color: String): Any => Unit = x =>
    println(setColor(color)(x))

  /** show duration time with loading message and only get data */
  def showTime[T](msg: String, f: => T): T = time(msg, f)._2

  /** catch exceptions with Option[_] */
  def optional[T](f: => T): Option[T] = try Some(f)
  catch { case e: Throwable => None }

  /** get catched error message */
  def getError[T](f: => T): Option[Throwable] =
    try { f; None }
    catch { case e: Throwable => Some(e) }

  /** get indentation */
  def getIndent(str: String): Int =
    "[ ]+".r.findFirstIn(str).fold(-1)(_.length)

  /** date format string */
  def dateStr: String = (SimpleDateFormat("yyMMdd_HH_mm")
    .format(Date()))
    .toString

  /** get fail/warn/pass message */
  def failMsg(msg: String): String = setColor(RED)("[FAIL] " + msg)
  def warnMsg(msg: String): String = setColor(YELLOW)("[WARN] " + msg)
  def passMsg(msg: String): String = setColor(GREEN)("[PASS] " + msg)

  /** split lists by a separator */
  def splitBy[T](list: List[T], sep: T): List[List[T]] = {
    @annotation.tailrec
    def aux(xs: List[T], revAcc: List[List[T]]): List[List[T]] = xs match {
      case Nil => revAcc.reverse
      case h :: t =>
        val (pref, suff) = (if (h == sep) xs.tail else xs).span(_ != sep)
        aux(suff, pref :: revAcc)
    }
    aux(list, Nil)
  }

  /** slice list by offset and stride */
  def slice[T](l: List[T], offset: Int, stride: Int): List[T] = {
    l.zipWithIndex.flatMap {
      case (e, idx) if (idx - offset) % stride == 0 => Some(e)
      case _                                        => None
    }
  }

  /** trim only right */
  def trimRight(str: String): String =
    str.reverse.span(_ == ' ')._2.reverse

  /** normalize strings */
  def normStr(str: String): String = str
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", "\\n")
    .replace("\b", "\\b")

  private def rand = Random()

  /** randomly choose an element in a list */
  def choose[T](seq: Seq[T]): T = seq(rand.nextInt(seq.length))

  /** randomly choose an element in a list and return it with its index */
  def chooseWithIndex[T](seq: Seq[T]): (T, Int) = {
    val idx = rand.nextInt(seq.length); (seq(idx), idx)
  }

  /** random boolean */
  def randBool: Boolean = rand.nextBoolean

  /** random integer */
  def randInt(n: Int): Int = rand.nextInt(n)

  /** randomly choose an element in a list with different weights */
  def weightedChoose[T](arr: Array[(T, Int)]): T = {
    val _arr = arr.filter(_._2 != 0)
    val n = rand.nextInt(_arr.map(_._2).sum) + 1
    def aux(idx: Int = 0, acc: Int = 0): T = {
      val curr = acc + _arr(idx)._2
      if (curr >= n) _arr(idx)._1
      else aux(idx + 1, curr)
    }
    aux()
  }

  /** stringify */
  def stringify[T](t: T)(using rule: Appender.Rule[T]): String =
    rule(Appender(), t).toString

  /** get a string for success ratio */
  def ratioString(pass: Int, total: Int): String = {
    val fail = total - pass
    f"P/F/T = $pass/$fail/$total ${ratioSimpleString(pass, total)}"
  }

  /** get a simple string for success ratio */
  def ratioSimpleString(pass: Int, total: Int): String = {
    val percent = pass / total.toDouble * 100
    f"($percent%.2f%%)"
  }

  /** equality between doubles */
  def doubleEquals(left: Double, right: Double): Boolean =
    if (left.isNaN && right.isNaN) true
    else if (isNegZero(left) && !isNegZero(right)) false
    else if (!isNegZero(left) && isNegZero(right)) false
    else left == right

  /** negative zero check */
  def isNegZero(double: Double): Boolean = (1 / double).isNegInfinity

  /** get a unique value from an iterable structure */
  def getUnique[T](
    iter: Iterable[T],
    f: T => Boolean,
    name: String = "element",
  ): T = iter.filter(f).toList match
    case List(elem) => elem
    case Nil        => error(s"no $name")
    case _          => error(s"multiple ${name}s")

  /** extensions for integers */
  extension (int: Int) {
    def toOrdinal: String = int match
      case 1 => "first"
      case 2 => "second"
      case 3 => "third"
      case 4 => "fourth"
      case 5 => "fifth"
      case 6 => "sixth"
      case 7 => "seventh"
      case 8 => "eighth"
      case 9 => "ninth"
      case n =>
        n.toString + (n % 10 match
          case 1 => "st"
          case 2 => "nd"
          case 3 => "rd"
          case _ => "th"
        )
  }

  /** extensions for integers */
  extension (str: String) {
    def toIntFromOrdinal: Option[Int] = optional(str match
      case "first"   => 1
      case "second"  => 2
      case "third"   => 3
      case "fourth"  => 4
      case "fifth"   => 5
      case "sixth"   => 6
      case "seventh" => 7
      case "eighth"  => 8
      case "ninth"   => 9
      case _         => str.dropRight(2).toInt,
    )
    def indefArticle: String = str.headOption.fold("")(c =>
      if ("[aeiou]".r.matches(c.toLower.toString)) "an"
      else "a",
    )
    def pluralPostfix: String = "s" // TODO
    def withIndefArticle: String = str.indefArticle + " " + str
    def withArticle(plural: Boolean = false): String =
      if (plural) str + str.pluralPostfix
      else str.indefArticle + " " + str
  }
}
