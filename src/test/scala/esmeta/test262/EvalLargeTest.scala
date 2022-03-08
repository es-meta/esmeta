package esmeta.test262

import esmeta.*
import esmeta.error.*
import esmeta.js.*
import esmeta.js.util.*
import esmeta.test262.NormalConfig
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException

class EvalLargeTest extends Test262Test {
  val name: String = "test262ParseTest"

  import Test262Test.test262.*

  // parser timeout
  val PARSE_TIMEOUT = 100 // second

  // progress bar
  lazy val progress = ProgressBar("test262 eval test", config.normal)

  // summary
  lazy val summary = progress.summary

  // registration
  def init: Unit = check(name) {
    mkdir(logDir)
    dumpFile(spec.version, s"$logDir/ecma262-version")
    dumpFile(currentVersion(BASE_DIR), s"$logDir/esmeta-version")
    summary.timeouts.setPath(s"$logDir/eval-timeout.log")
    summary.yets.setPath(s"$logDir/eval-yet.log")
    summary.fails.setPath(s"$logDir/eval-fail.log")
    summary.passes.setPath(s"$logDir/eval-pass.log")
    for (config <- progress) {
      val NormalConfig(name, includes) = config
      val jsName = s"$TEST262_TEST_DIR/$name"
      getError {
        // parse test262 including harness
        val includeStmts = includes.foldLeft(basicStmts) {
          case (li, s) =>
            for {
              x <- li
              y <- getInclude(s)
            } yield x ++ y
        } match {
          case Right(l)  => l
          case Left(msg) => throw NotSupported(msg)
        }
        val parsed = timeout(parseFile(jsName), PARSE_TIMEOUT)
        val stmts = includeStmts ++ flattenStmt(parsed)
        val merged = mergeStmt(stmts)
        val sourceText = merged.toString(grammar = Some(spec.grammar)).trim

        // run interpreter
        JSTest.evalTest(sourceText, cachedAst = Some(merged))
        summary.passes += name
      }.foreach {
        case NotSupported(msg)   => summary.yets += s"$name: $msg"
        case _: TimeoutException => summary.timeouts += name
        case _: Throwable        => summary.fails += name
      }
    }
    summary.close
    dumpFile(summary, s"$logDir/parse-summary")
    if (summary.fail > 0) fail(s"${summary.fail} tests are failed.")
  }
  init
}
