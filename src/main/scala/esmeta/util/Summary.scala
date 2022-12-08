package esmeta.util

import esmeta.LINE_SEP
import esmeta.error.NotSupported
import esmeta.error.NotSupported.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import java.io.PrintWriter

class Summary {
  import Summary.*

  /** not yet supported */
  val yets: Elem = MapElem()
  def yet: Int = yets.size

  /** timeout */
  val timeouts: Elem = SeqElem()
  def timeout: Int = timeouts.size

  /** fail */
  val fails: Elem = SeqElem()
  def fail: Int = fails.size

  /** pass */
  val passes: Elem = SeqElem()
  def pass: Int = passes.size

  /** dump results */
  def dumpTo(baseDir: String): Unit =
    if (!yets.isEmpty) yets.dumpTo("yet", s"$baseDir/yets.json")
    if (!timeouts.isEmpty) timeouts.dumpTo("timeout", s"$baseDir/timeouts.json")
    if (!fails.isEmpty) fails.dumpTo("fail", s"$baseDir/fails.json")
    if (!passes.isEmpty) passes.dumpTo("pass", s"$baseDir/passes.json")

  /** time */
  var time: Time = Time()

  /** total cases */
  def total: Int = yet + timeout + fail + pass

  /** supported total cases */
  def supported: Int = timeout + fail + pass

  /** pass rate */
  def passRate: Double = pass.toDouble / supported
  def passPercent: Double = passRate * 100

  /** get simple string */
  def simpleString: String =
    var pairs = List(("P", pass))
    if (fail > 0) pairs ::= ("F", fail)
    if (yet > 0) pairs ::= ("Y", yet)
    if (timeout > 0) pairs ::= ("T", timeout)
    val (names, counts) = pairs.unzip
    val namesStr = names.mkString("/")
    val countsStr = counts.map(x => f"$x%,d").mkString("/")
    f"$namesStr = $countsStr ($passPercent%2.2f%%)"

  /** conversion to string */
  override def toString: String = total match
    case 0 => "[Summary] no targets."
    case 1 =>
      if (yet == 1) "YET"
      else if (timeout == 1) "TIMEOUT"
      else if (fail == 1) "FAIL"
      else "PASS"
    case _ =>
      val app = Appender()
      app >> f"- time: $time" >> LINE_SEP
      app >> f"- total: $total%,d" >> LINE_SEP
      if (yet > 0) app >> f"  - yet: $yet%,d" >> LINE_SEP
      if (timeout > 0) app >> f"  - timeout: $timeout%,d" >> LINE_SEP
      if (fail > 0) app >> f"  - fail: $fail%,d" >> LINE_SEP
      if (pass > 0) app >> f"  - pass: $pass%,d" >> LINE_SEP
      app >> f"- pass-rate: $pass%,d/$supported%,d ($passPercent%2.2f%%)"
      app.toString
}

object Summary {

  /** summary elements */
  sealed trait Elem {

    /** all elements */
    def all: List[String] = this match
      case SeqElem(elems) => elems.toList
      case MapElem(map) =>
        for { (_, elem) <- map.toList; elem <- elem.all } yield elem

    /** empty check */
    def isEmpty: Boolean = this match
      case SeqElem(elems) => elems.isEmpty
      case MapElem(map)   => map.forall { case (_, elem) => elem.isEmpty }

    /** size */
    def size: Int = this match
      case SeqElem(elems) => elems.size
      case MapElem(map)   => (for { (_, elem) <- map } yield elem.size).sum

    /** add data */
    def add(data: String, reason: Reason): Unit = add(data, List(reason))
    def add(data: String, reasons: ReasonPath = Nil): Unit =
      (this, reasons) match
        case (elem @ SeqElem(_), Nil) => elem.elems :+= data
        case (elem @ MapElem(_), reason :: remain) =>
          val subElem = elem.map.getOrElse(
            reason, {
              val newSubElem = if (remain.isEmpty) SeqElem() else MapElem()
              elem.map += reason -> newSubElem
              newSubElem
            },
          )
          subElem.add(data, remain)
        case _ => ???

    /** dump results */
    def dumpTo(name: String, filename: String): Unit = dumpJson(
      name = s"$name cases",
      data = toJson,
      filename = filename,
      noSpace = false,
    )

    /** conversion to JSON */
    def toJson: Json = this match
      case SeqElem(elems) => Json.fromValues(elems.map(Json.fromString))
      case MapElem(map) =>
        Json.fromFields(map.toList.map { case (r, e) => r -> e.toJson })
  }

  /** sequential elements */
  case class SeqElem(var elems: Vector[String] = Vector()) extends Elem

  /** mapping from reasons to elements */
  case class MapElem(var map: Map[Reason, Elem] = Map()) extends Elem
}
