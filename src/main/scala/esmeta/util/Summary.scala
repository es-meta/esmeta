package esmeta.util

import esmeta.error.NotSupported.*
import esmeta.util.Appender.Rule
import esmeta.util.Summary.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.IterableHasAsScala

case class Summary(
  notSupported: Elem = Elem(), // not yet supported elements
  timeout: Elem = Elem(), // timeout elements
  fail: Elem = Elem(), // failed elements
  pass: Elem = Elem(), // passed elements
) {

  // the number of not yet supported elements
  def notSupportedCount: Int = notSupported.size

  // the number of timeout elements
  def timeoutCount: Int = timeout.size

  // the number of failed elements
  def failCount: Int = fail.size

  // the number of passed elements
  def passCount: Int = pass.size

  /** dump results */
  def dumpTo(baseDir: String): Unit =
    if (!notSupported.isEmpty)
      notSupported.dumpTo("not-supported", s"$baseDir/not-supported.json")
    if (!timeout.isEmpty) timeout.dumpTo("timeout", s"$baseDir/timeout.json")
    if (!fail.isEmpty) fail.dumpTo("fail", s"$baseDir/fail.json")
    if (!pass.isEmpty) pass.dumpTo("pass", s"$baseDir/pass.json")

  /** time */
  var time: Time = Time()

  /** total cases */
  def total: Int = notSupportedCount + timeoutCount + failCount + passCount

  /** supported total cases */
  def supported: Int = timeoutCount + failCount + passCount

  /** pass rate */
  def passRate: Double = passCount.toDouble / supported
  def passPercent: Double = passRate * 100

  /** get simple string */
  def simpleString: String = simpleString(true)
  def simpleString(detail: Boolean = true): String =
    def getMap(pairs: (String, Int)*): Map[String, Int] = (for {
      (x, count) <- pairs
      if count > 0
    } yield x -> count).toMap
    val map = getMap(
      "P" -> passCount,
      "F" -> failCount,
      "N" -> notSupportedCount,
      "T" -> timeoutCount,
    )
    def N(xs: String*) = xs.filter(map.contains)
    def C(xs: String*) = xs.flatMap(map.get)
    val app = Appender()
    if (detail)
      app >> N("P", "F", "T", "N").mkString(":") >> " = "
      app >> C("P", "F", "T", "N").map(x => f"$x%,d").mkString(":") >> " => "
    app >> "P/" >> N("P", "F", "T").mkString("+") >> " = "
    app >> f"$passCount%,d/${C("P", "F", "T").sum}%,d ($passPercent%2.2f%%)"
    app.toString

  /** conversion to string */
  override def toString: String = total match
    case 0 => "[Summary] no targets."
    case 1 =>
      if (notSupportedCount == 1) "NOT-SUPPORTED"
      else if (timeoutCount == 1) "TIMEOUT"
      else if (failCount == 1) "FAIL"
      else "PASS"
    case _ =>
      val app = Appender()
      app >> f"- time: $time"
      (app :> "total" -> total).wrap("", "") {
        if (notSupportedCount > 0) app :> "not-supported (N)" -> notSupported
        if (timeoutCount > 0) app :> "timeout (T)" -> timeoutCount
        if (failCount > 0) app :> "fail (F)" -> failCount
        if (passCount > 0) app :> "pass (P)" -> passCount
      }
      app :> f"- pass-rate: ${simpleString(detail = false)}"
      app.toString

  private given elemRuleSimple: Rule[Elem] = (app, elem) => app >> elem.size
  private given Rule[(Reason, Elem)] = (app, pair) =>
    val (reason, elem) = pair
    app >> reason >> " -> " >> elem
  private given Rule[(String, Int)] = {
    case (app, (name, count)) => app >> f"- $name: $count%,d"
  }
}

object Summary {

  /** summary elements */
  case class Elem(
    seq: BlockingQueue[String] = LinkedBlockingQueue(),
    map: TrieMap[Reason, Elem] = TrieMap(),
  ) {

    /** all elements */
    def all: List[String] =
      val mapElems =
        for { (_, elem) <- map.toList; elem <- elem.all } yield elem
      val seqElems = seq.asScala.toList
      seqElems ++ mapElems

    /** empty check */
    def isEmpty: Boolean =
      seq.isEmpty && map.forall { case (_, elem) => elem.isEmpty }

    /** size */
    def size: Int =
      seq.size + (for { (_, elem) <- map } yield elem.size).sum

    /** add data */
    def add(data: String, reason: Reason): Unit = add(data, List(reason))
    def add(data: String, reasons: ReasonPath = Nil): Unit = reasons match
      case Nil => seq.offer(data)
      case reason :: remain =>
        map.getOrElseUpdate(reason, Elem()).add(data, remain)

    /** dump results */
    def dumpTo(name: String, filename: String): Unit = dumpJson(
      name = s"$name cases",
      data = toJson,
      filename = filename,
      noSpace = false,
    )

    /** conversion to JSON */
    def toJson: Json =
      lazy val seqJson = Json.fromValues(seq.asScala.map(Json.fromString))
      if (map.isEmpty) seqJson
      else
        val mapValues = map.toList.map { case (r, e) => r -> e.toJson }
        Json.fromFields(
          if (seq.isEmpty) mapValues else mapValues :+ ("others" -> seqJson),
        )
  }
}
