package esmeta.util

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import esmeta.LINE_SEP
import esmeta.util.BaseUtils.*

// progress bar
case class ProgressBar[T](
  msg: String,
  iterable: Iterable[T],
  getName: (T, Int) => String = (_: T, idx) => s"${idx.toOrdinal} element",
  errorHandler: (Throwable, Summary, String) => Unit = (_, summary, name) =>
    summary.fails += name,
) extends Iterable[T] {
  // summary
  val summary = Summary()

  // postfix for summary
  def postfix = (
    if (summary.total == summary.pass) ""
    else s" - ${summary.simpleString}"
  ) + s" [${summary.timeString}]"

  // bar length
  val BAR_LEN = 40

  // update interval
  val term = 1000 // 1 second

  // iterators
  final def iterator: Iterator[T] = iterable.iterator

  // foreach function
  override def foreach[U](f: T => U): Unit = {
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
        f"[$progress] $percent%2.2f%% ($count%,d/$size%,d)$postfix"
      print("\r" + msg)
      if (count != size) { Thread.sleep(term); show }
      else println
    }
    println(s"- $msg...")

    val future = show
    for ((x, idx) <- iterable.zipWithIndex)
      val name = getName(x, idx)
      getError {
        f(x)
        summary.passes += name
      }.map(errorHandler(_, summary, name))
      gcount += 1
    updateTime

    // close all print writers
    summary.close

    Thread.sleep(term)
  }

  // progress bar character
  val BAR = "#"
}
