package esmeta.test262

import esmeta.*
import esmeta.error.NotSupported
import esmeta.es.*
import esmeta.es.util.*
import esmeta.parser.ESParser
import esmeta.spec.Spec
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** data in Test262 */
case class Test262(
  version: Test262.Version,
  spec: Spec,
) {

  /** cache for parsing results for necessary harness files */
  lazy val getHarness = cached(name =>
    val filename = s"$TEST262_DIR/harness/$name"
    () => parseFile(filename).flattenStmt,
  )

  /** all Test262 tests */
  lazy val allTests: List[MetaData] = MetaData.fromDir(TEST262_TEST_DIR)

  /** test262 test configuration */
  lazy val allTestFilter: TestFilter = TestFilter(allTests)

  /** configuration summary for applicable tests */
  lazy val config: ConfigSummary = allTestFilter.summary

  /** configuration summary for manually selected tests */
  lazy val manualConfig: ConfigSummary = allTestFilter.manualSummary

  /** basic harness files */
  lazy val basicHarness = getHarness("assert.js")() ++ getHarness("sta.js")()

  /** load test262 */
  def loadTest(filename: String): Ast =
    loadTest(filename, MetaData(filename).includes)

  /** load test262 with harness files */
  def loadTest(filename: String, includes: List[String]): Ast =
    // load harness
    val harnessStmts = includes.foldLeft(basicHarness)(_ ++ getHarness(_)())

    // merge with harnesses
    val stmts = flattenStmt(parseFile(filename))
    mergeStmt(harnessStmts ++ stmts)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // parse ECMAScript file
  private lazy val scriptParser = spec.scriptParser
  private def parseFile(filename: String): Ast = scriptParser.fromFile(filename)
}
object Test262 extends Git(TEST262_DIR)
