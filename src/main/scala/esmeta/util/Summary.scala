package esmeta.util

import java.io.PrintWriter
import esmeta.LINE_SEP

class Summary {
  // not yet supported
  val yets: SummaryElem = new SummaryElem
  def yet: Int = yets.size

  // timeout
  val timeouts: SummaryElem = new SummaryElem
  def timeout: Int = timeouts.size

  // fail
  val fails: SummaryElem = new SummaryElem
  def fail: Int = fails.size

  // pass
  val passes: SummaryElem = new SummaryElem
  def pass: Int = passes.size

  // close all print writers
  def close: Unit = { yets.close; timeouts.close; fails.close; passes.close }

  // time
  var timeMillis: Long = 0L
  def timeHours = timeMillis / 3600000.0

  // total cases
  def total: Int = yet + timeout + fail + pass

  // supported total cases
  def supported: Int = fail + pass

  // pass rate
  def passRate: Double = pass.toDouble / supported
  def passPercent: Double = passRate * 100

  // get simple string
  def simpleString: String = {
    var pairs = List(("P", pass))
    if (fail > 0) pairs ::= ("F", fail)
    if (yet > 0) pairs ::= ("Y", yet)
    if (timeout > 0) pairs ::= ("T", timeout)
    val (names, counts) = pairs.unzip
    val namesStr = names.mkString("/")
    val countsStr = counts.mkString("/")
    f"$namesStr = $countsStr ($passPercent%2.2f%%)"
  }

  // conversion to string
  override def toString: String = {
    val app = new Appender
    app >> f"time: $timeMillis%,d ms ($timeHours%.1f hours)" >> LINE_SEP
    app >> f"total: $total%,d" >> LINE_SEP
    if (yet > 0) app >> f"- yet: $yet%,d" >> LINE_SEP
    app >> f"- fail: $fail%,d" >> LINE_SEP
    app >> f"- pass: $pass%,d" >> LINE_SEP
    app >> f"pass-rate: $pass%,d/$supported%,d ($passPercent%2.2f%%)"
    app.toString
  }
}

class SummaryElem {
  // vector
  private var vector: Vector[String] = Vector()

  // print writer
  var nfOpt: Option[PrintWriter] = None
  def setPath(nf: PrintWriter): Unit = nfOpt = Some(nf)
  def setPath(filename: String): Unit = setPath(
    SystemUtils.getPrintWriter(filename),
  )
  def close: Unit = nfOpt.map(_.close())

  // size
  def size: Int = vector.size

  // add data
  def +=(data: String): Unit = {
    nfOpt.map(nf => {
      nf.println(data)
      nf.flush()
    })
    vector :+= data
  }
}
