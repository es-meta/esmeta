package esmeta.util

import java.io.PrintWriter
import esmeta.LINE_SEP

class Summary {
  // not yet supported
  val yets: SummaryElem = SummaryElem()
  def yet: Int = yets.size

  // timeout
  val timeouts: SummaryElem = SummaryElem()
  def timeout: Int = timeouts.size

  // fail
  val fails: SummaryElem = SummaryElem()
  def fail: Int = fails.size

  // pass
  val passes: SummaryElem = SummaryElem()
  def pass: Int = passes.size

  // close all print writers
  def close: Unit = { yets.close; timeouts.close; fails.close; passes.close }

  // time
  var time: Time = Time()

  // total cases
  def total: Int = yet + timeout + fail + pass

  // supported total cases
  def supported: Int = timeout + fail + pass

  // pass rate
  def passRate: Double = pass.toDouble / supported
  def passPercent: Double = passRate * 100

  // get simple string
  def simpleString: String =
    var pairs = List(("P", pass))
    if (fail > 0) pairs ::= ("F", fail)
    if (yet > 0) pairs ::= ("Y", yet)
    if (timeout > 0) pairs ::= ("T", timeout)
    val (names, counts) = pairs.unzip
    val namesStr = names.mkString("/")
    val countsStr = counts.map(x => f"$x%,d").mkString("/")
    f"$namesStr = $countsStr ($passPercent%2.2f%%)"

  // conversion to string
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

class SummaryElem {
  // elements
  var elements: Vector[String] = Vector()

  // print writer
  var nfOpt: Option[PrintWriter] = None
  def setPath(nf: PrintWriter): Unit = nfOpt = Some(nf)
  def setPath(filename: String): Unit =
    setPath(SystemUtils.getPrintWriter(filename))
  def close: Unit = nfOpt.map(_.close())

  // size
  def size: Int = elements.size

  // add data
  def +=(data: String): Unit =
    for (nf <- nfOpt)
      nf.println(data)
      nf.flush()
    elements :+= data
}
