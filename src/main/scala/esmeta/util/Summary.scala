package esmeta.util

import esmeta.LINE_SEP
import esmeta.error.NotSupported
import esmeta.error.NotSupported.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap, ListBuffer}
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import java.io.PrintWriter

class Summary {
  import Summary.*

  /** not yet supported */
  val notSupported: Elem = Elem()
  def notSupportedCount: Int = notSupported.size

  /** timeout */
  val timeout: Elem = Elem()
  def timeoutCount: Int = timeout.size

  /** fail */
  val fail: Elem = Elem()
  def failCount: Int = fail.size

  /** pass */
  val pass: Elem = Elem()
  def passCount: Int = pass.size

  /** dump results */
  def dumpTo(baseDir: String): Unit =
    if (!notSupported.isEmpty)
      notSupported.dumpTo("notsupported", s"$baseDir/notsupported.json")
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
  def simpleString: String =
    var pairs = List(("P", passCount))
    if (failCount > 0) pairs ::= ("F", failCount)
    if (notSupportedCount > 0) pairs ::= ("N", notSupportedCount)
    if (timeoutCount > 0) pairs ::= ("T", timeoutCount)
    val (names, counts) = pairs.unzip
    val namesStr = names.mkString("/")
    val countsStr = counts.map(x => f"$x%,d").mkString("/")
    f"$namesStr = $countsStr ($passPercent%2.2f%%)"

  /** conversion to string */
  override def toString: String = total match
    case 0 => "[Summary] no targets."
    case 1 =>
      if (notSupportedCount == 1) "NOTSUPPORTED"
      else if (timeoutCount == 1) "TIMEOUT"
      else if (failCount == 1) "FAIL"
      else "PASS"
    case _ =>
      val app = Appender()
      app >> f"- time: $time" >> LINE_SEP
      app >> f"- total: $total%,d" >> LINE_SEP
      if (notSupportedCount > 0)
        app >> f"  - notsupported: $notSupportedCount%,d" >> LINE_SEP
        val notsupported_subsection =
          notSupported.map.toList
            .sortBy(-_._2.size)
            .map { (reason, elem) =>
              s"$reason: ${elem.size}"
            }
            .mkString(s"$LINE_SEP    - ")
        app >> s"    - $notsupported_subsection" >> LINE_SEP
      if (timeoutCount > 0)
        app >> f"  - timeout: $timeoutCount%,d" >> LINE_SEP
      if (failCount > 0) app >> f"  - fail: $failCount%,d" >> LINE_SEP
      if (passCount > 0) app >> f"  - pass: $passCount%,d" >> LINE_SEP
      app >> f"- pass-rate: $passCount%,d/$supported%,d ($passPercent%2.2f%%)"
      app.toString
}

object Summary {

  /** summary elements */
  case class Elem(
    seq: ListBuffer[String] = ListBuffer(),
    map: MMap[Reason, Elem] = MMap(),
  ) {

    /** all elements */
    def all: List[String] =
      val mapElems =
        for { (_, elem) <- map.toList; elem <- elem.all } yield elem
      val seqElems = seq.toList
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
      case Nil => seq.append(data)
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
      lazy val seqJson = Json.fromValues(seq.map(Json.fromString))
      if (map.isEmpty) seqJson
      else
        val mapValues = map.toList.map { case (r, e) => r -> e.toJson }
        Json.fromFields(
          if (seq.isEmpty) mapValues else mapValues :+ ("others" -> seqJson),
        )
  }
}
