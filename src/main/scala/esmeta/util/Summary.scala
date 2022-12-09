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
  val notsupported: Elem = Elem()
  def notsupported_count: Int = notsupported.size

  /** timeout */
  val timeout: Elem = Elem()
  def timeout_count: Int = timeout.size

  /** fail */
  val fail: Elem = Elem()
  def fail_count: Int = fail.size

  /** pass */
  val pass: Elem = Elem()
  def pass_count: Int = pass.size

  /** dump results */
  def dumpTo(baseDir: String): Unit =
    if (!notsupported.isEmpty)
      notsupported.dumpTo("notsupported", s"$baseDir/notsupported.json")
    if (!timeout.isEmpty) timeout.dumpTo("timeout", s"$baseDir/timeout.json")
    if (!fail.isEmpty) fail.dumpTo("fail", s"$baseDir/fail.json")
    if (!pass.isEmpty) pass.dumpTo("pass", s"$baseDir/pass.json")

  /** time */
  var time: Time = Time()

  /** total cases */
  def total: Int = notsupported_count + timeout_count + fail_count + pass_count

  /** supported total cases */
  def supported: Int = timeout_count + fail_count + pass_count

  /** pass rate */
  def passRate: Double = pass_count.toDouble / supported
  def passPercent: Double = passRate * 100

  /** get simple string */
  def simpleString: String =
    var pairs = List(("P", pass_count))
    if (fail_count > 0) pairs ::= ("F", fail_count)
    if (notsupported_count > 0) pairs ::= ("N", notsupported_count)
    if (timeout_count > 0) pairs ::= ("T", timeout_count)
    val (names, counts) = pairs.unzip
    val namesStr = names.mkString("/")
    val countsStr = counts.map(x => f"$x%,d").mkString("/")
    f"$namesStr = $countsStr ($passPercent%2.2f%%)"

  /** conversion to string */
  override def toString: String = total match
    case 0 => "[Summary] no targets."
    case 1 =>
      if (notsupported_count == 1) "NOTSUPPORTED"
      else if (timeout_count == 1) "TIMEOUT"
      else if (fail_count == 1) "FAIL"
      else "PASS"
    case _ =>
      val app = Appender()
      app >> f"- time: $time" >> LINE_SEP
      app >> f"- total: $total%,d" >> LINE_SEP
      if (notsupported_count > 0)
        app >> f"  - notsupported: $notsupported_count%,d" >> LINE_SEP
        val notsupported_subsection =
          notsupported.map.toList
            .sortBy(-_._2.size)
            .map { (reason, elem) =>
              s"$reason: ${elem.size}"
            }
            .mkString(s"$LINE_SEP    - ")
        app >> s"    - $notsupported_subsection" >> LINE_SEP
      if (timeout_count > 0)
        app >> f"  - timeout: $timeout_count%,d" >> LINE_SEP
      if (fail_count > 0) app >> f"  - fail: $fail_count%,d" >> LINE_SEP
      if (pass_count > 0) app >> f"  - pass: $pass_count%,d" >> LINE_SEP
      app >> f"- pass-rate: $pass_count%,d/$supported%,d ($passPercent%2.2f%%)"
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
