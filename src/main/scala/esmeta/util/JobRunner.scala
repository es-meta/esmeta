package esmeta.util

import esmeta.LINE_SEP
import esmeta.error.NotSupported.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy as CP}
import esmeta.util.SystemUtils.{concurrent as doConcurrent, fixedThread}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicInteger

/* job runner with built-in progress bar support */
abstract class JobRunner[T, U](
  targets: Iterable[T],
  concurrent: CP,
  showProgressBar: Boolean,
) {

  def job(t: T, idx: Int): Unit
  def postJob: U

  def barPostfix: String = ""

  lazy val countBase: Int = 0
  final val count = AtomicInteger(countBase)
  final val totalSize = countBase + targets.size
  final lazy private val startTime = System.currentTimeMillis
  final def elapsedTime: Time = Time(System.currentTimeMillis - startTime)

  /** a function which is called every tick */
  def tick: Unit = ()

  private def printProgress: Unit = {
    val c = count.get
    val percent = c.toDouble / totalSize * 100
    val len = c * BAR_LEN / totalSize
    val bars = (BAR * len) + (" " * (BAR_LEN - len))
    val msg =
      f"[$bars] $percent%2.2f%% ($c%,d/$totalSize%,d)$barPostfix"
    print("\r" + msg)
  }

  protected def run: U = {

    def show: Future[Unit] = Future {
      if (count.get != totalSize) {
        printProgress
        tick
        Thread.sleep(INTERVAL)
        show
      } else { println() }
    }

    val jobClo = for (x <- targets) yield () =>
      val idx = count.getAndIncrement()
      job(x, idx)

    if (showProgressBar) show

    concurrent match
      case CP.Single => jobClo.foreach(_.apply)
      case CP.Fixed(n) =>
        val (service, eCtxt) = fixedThread(n)
        try {
          doConcurrent(jobClo)(using eCtxt)
        } finally {
          service.shutdown()
        }
      case CP.Auto => doConcurrent(jobClo)

    if (showProgressBar) { printProgress; tick; println() }
    postJob
  }

  lazy val result = run

  /* progress bar character */
  private val BAR = "#"

  /* progress bar length */
  private val BAR_LEN = 40

  /* update interval, defaults to 1 sec */
  private val INTERVAL = 1000
}
