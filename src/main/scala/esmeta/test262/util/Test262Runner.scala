package esmeta.test262.util

import esmeta.error.NotSupported.ReasonPath
import esmeta.test262.Test
import esmeta.util.{ConcurrentPolicy as CP, JobRunner}
import esmeta.util.BaseUtils.*

abstract class Test262Runner(
  val msg: String,
  val targets: Iterable[Test],
  val notSupported: Iterable[(Test, ReasonPath)],
  val concurrent: CP,
  val showProgressBar: Boolean,
  val verbose: Boolean = true,
  val detail: Boolean = true,
) extends JobRunner[Test, Summary](
    targets = targets,
    concurrent = concurrent,
    showProgressBar = showProgressBar,
  ) {

  lazy val getName: (Test, Int) => String

  override lazy val countBase = notSupported.size

  // summary
  lazy val summary =
    val elem = Summary.Elem()
    for { ((x, reasonPath), idx) <- notSupported.zipWithIndex }
      elem.add(getName(x, idx), reasonPath)
    Summary(notSupported = elem)

  def runTest(t: Test): Unit

  override def tick: Unit = summary.time = elapsedTime

  final def job(t: Test, idx: Int): Unit = {
    val name = getName(t, idx)
    getError {
      runTest(t)
      summary.pass.add(name)
    }.map(errorHandler(_, summary, name, t))
  }

  def postJob: Summary = summary

  def errorHandler(
    error: Throwable,
    summary: Summary,
    name: String,
    test: Test,
  ): Unit = summary.fail.add(name)

  /* postfix for summary */
  override def barPostfix =
    (if (detail)
       if (summary.total == summary.passCount) ""
       else s" - ${summary.simpleString}"
     else "") + s" [${summary.time.simpleString}]"

  override def run: Summary =
    // pre-jobs
    if (verbose)
      println(f"- $msg (total: $totalSize%,d)")
      if (countBase > 0)
        println(f"  - $countBase%,d targets are detected as not supported.")
        println("    (The not supported targets can be dynamically detected.)")
      if (detail)
        println(s"  - P: passed targets")
        println(s"  - F: failed targets")
        println(s"  - T: timeout targets")
        println(s"  - N: not supported targets")
    super.run

}
