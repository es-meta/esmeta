package esmeta.util

import esmeta.LINE_SEP
import esmeta.error.NotSupported.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.{concurrent => doConcurrent, fixedThread}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicInteger

// progress bar
case class ProgressBar[T](
  msg: String,
  iterable: Iterable[T],
  notSupported: Iterable[(T, ReasonPath)] = Map[T, ReasonPath](),
  getName: (T, Int) => String = (_: T, idx) => s"${idx.toOrdinal} element",
  errorHandler: (Throwable, Summary, String) => Unit = (_, summary, name) =>
    summary.fail.add(name),
  timeLimit: Option[Int] = None, // seconds
  verbose: Boolean = true,
  detail: Boolean = true,
  concurrent: CP = CP.Single,
) extends Iterable[T] {
  // summary
  val summary =
    val elem = Summary.Elem()
    for { ((x, reasonPath), idx) <- notSupported.zipWithIndex }
      elem.add(getName(x, idx), reasonPath)
    Summary(notSupported = elem)

  // postfix for summary
  def postfix =
    (if (detail)
       if (summary.total == summary.passCount) ""
       else s" - ${summary.simpleString}"
     else "") + s" [${summary.time.simpleString}]"

  // bar length
  val BAR_LEN = 40

  // update interval
  val term = 1000 // 1 second

  // iterators
  final def iterator: Iterator[T] = iterable.iterator

  // size
  val notSupportedSize = notSupported.size
  val baseSize = notSupportedSize
  override val size: Int = baseSize + iterable.size

  // foreach function
  override def foreach[U](f: T => U): Unit = {
    val gcount = AtomicInteger(baseSize)
    val start = System.currentTimeMillis
    def updateTime: Unit =
      summary.time = Time(System.currentTimeMillis - start)

    def show: Future[Unit] = Future {
      val count = gcount.get
      val percent = count.toDouble / size * 100
      val len = count * BAR_LEN / size
      val bars = (BAR * len) + (" " * (BAR_LEN - len))
      updateTime
      val msg =
        f"[$bars] $percent%2.2f%% ($count%,d/$size%,d)$postfix"
      print("\r" + msg)
      if (count != size) { Thread.sleep(term); show }
      else println
    }

    if (verbose)
      println(f"- $msg... (total: $size%,d)")
      if (baseSize > 0)
        println(f"  - $baseSize%,d targets are detected as not supported.")
        println("    (The not supported targets can be dynamically detected.)")
      if (detail)
        println(s"  - P: passed targets")
        println(s"  - F: failed targets")
        println(s"  - T: timeout targets")
        println(s"  - N: not supported targets")
      show

    val tests = for ((x, idx) <- iterable.zipWithIndex) yield () =>
      val name = getName(x, baseSize + idx)
      getError {
        f(x)
        summary.pass.add(name)
      }.map(errorHandler(_, summary, name))
      gcount.incrementAndGet

    concurrent match
      case CP.Single => tests.foreach(_.apply)
      case CP.Fixed(n) =>
        val (service, eCtxt) = fixedThread(n)
        doConcurrent(tests)(using eCtxt)
        service.shutdown()
      case CP.Auto => doConcurrent(tests)

    updateTime

    if (verbose) Thread.sleep(term)
  }

  /** dump results */
  def dumpTo(baseDir: String): Unit = summary.dumpTo(baseDir)

  // progress bar character
  val BAR = "#"
}
object ProgressBar {
  def defaultGetName[T](x: T, idx: Int): String =
    s"${idx.toOrdinal} element"
}
