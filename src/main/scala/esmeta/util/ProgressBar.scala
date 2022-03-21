package esmeta.util

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import esmeta.LINE_SEP

// progress bar
case class ProgressBar[T](msg: String, seq: Iterable[T]) {
  // summary
  val summary = new Summary

  // postfix for summary
  def postfix =
    if (summary.total == 0) ""
    else s" - ${summary.simpleString}"

  // size
  val size = seq.size

  // bar length
  val BAR_LEN = 40

  // update interval
  val term = 1000 // 1 second

  // foreach function
  def foreach(f: T => Unit): Unit = {
    var gcount = 0
    val start = System.currentTimeMillis
    def updateTime: Unit = summary.timeMillis = System.currentTimeMillis - start
    def show: Future[Unit] = Future {
      val count = gcount
      val percent = count.toDouble / size * 100
      val len = count * BAR_LEN / size
      val progress = (BAR * len) + (" " * (BAR_LEN - len))
      updateTime
      val msg =
        f"[$progress] $percent%2.2f%% ($count%,d/$size%,d)$postfix (${summary.timeMillis}%,d ms ~= ${summary.timeHours}%.1f hours)"
      print("\r" + msg)
      if (count != size) { Thread.sleep(term); show }
      else println
    }
    println(msg + "...")
    val future = show
    seq.foreach(t => { f(t); gcount += 1 })
    updateTime
    Thread.sleep(term)
  }

  // progress bar character
  val BAR = "#"
}
