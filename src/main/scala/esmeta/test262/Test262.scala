package esmeta.test262

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported, InvalidExit, UnexpectedParseResult}
import esmeta.error.NotSupported.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.interpreter.Interpreter
import esmeta.parser.ESParser
import esmeta.state.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException

/** data in Test262 */
case class Test262(
  version: Test262.Version,
  cfg: CFG,
  withYet: Boolean = false,
) {

  /** cache for parsing results for necessary harness files */
  lazy val getHarness = cached(name =>
    val filename = s"$TEST262_DIR/harness/$name"
    () => parseFile(filename).flattenStmt,
  )

  /** test262 filter */
  lazy val testFilter: TestFilter = TestFilter(cfg.spec)

  /** all Test262 tests */
  lazy val allTests: List[Test] = Test.fromDir(TEST262_TEST_DIR)

  /** test262 test configuration */
  lazy val (allTargetTests, allRemoved) = testFilter(allTests, withYet)

  /** basic harness files */
  lazy val basicHarness = getHarness("assert.js")() ++ getHarness("sta.js")()

  /** specification */
  val spec = cfg.spec

  /** load test262 */
  def loadTest(filename: String): Ast =
    loadTest(filename, Test(filename).includes)

  /** load test262 with harness files */
  def loadTest(filename: String, includes: List[String]): Ast =
    // load harness
    val harnessStmts = includes.foldLeft(basicHarness)(_ ++ getHarness(_)())

    // merge with harnesses
    val stmts = flattenStmt(parseFile(filename))
    mergeStmt(harnessStmts ++ stmts)

  /** get tests */
  def getTests(
    paths: List[String] = Nil,
    log: Boolean = false,
  ): List[Test] =
    if (log) println("- Extracting tests of Test262 tests...")
    Test.fromDirs(paths match
      case Nil   => List(TEST262_TEST_DIR)
      case paths => paths,
    )

  /** get tests */
  def getProgressBar(
    name: String,
    targetTests: List[Test],
    removed: Iterable[(Test, ReasonPath)] = Nil,
    useProgress: Boolean = false,
    useErrorHandler: Boolean = true,
    concurrent: Boolean = false,
  ): ProgressBar[Test] = ProgressBar(
    msg = s"Run Test262 $name tests",
    iterable = targetTests,
    notSupported = removed,
    getName = (test, _) => test.relName,
    errorHandler = (e, summary, name) =>
      if (useErrorHandler) e match
        case NotSupported(reasons) => summary.notSupported.add(name, reasons)
        case _: TimeoutException   => summary.timeout.add(name)
        case e: Throwable          => summary.fail.add(name, e.getMessage)
      else throw e,
    verbose = useProgress,
    concurrent = concurrent,
  )

  /** interpreter test */
  def evalTest(
    paths: Iterable[String] = Nil,
    log: Boolean = false,
    useProgress: Boolean = false,
    useCoverage: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: Boolean = false,
  ): Summary = {
    // extract tests from paths
    val tests: List[Test] = getTests(paths.toList)

    // use error handler for multiple targets
    val multiple = tests.length > 1

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "eval",
      targetTests = targetTests,
      removed = removed,
      useProgress = useProgress,
      useErrorHandler = multiple,
      concurrent = concurrent,
    )

    // coverage with time limit
    lazy val cov = Coverage(
      cfg = cfg,
      test262 = Some(this),
      timeLimit = timeLimit,
    )

    // run tests with logging
    logForTests(
      name = "eval",
      progressBar = progressBar,
      postSummary = if (useCoverage) cov.toString else "",
      log = log && multiple,
    )(
      // check final execution status of each Test262 test
      check = test =>
        val filename = test.path
        val st =
          if (!useCoverage) evalFile(filename, log && !multiple, timeLimit)
          else cov.run(filename)
        val returnValue = st(GLOBAL_RESULT)
        if (returnValue != Undef) throw InvalidExit(returnValue)
      ,
      // dump coverage
      logDir => if (useCoverage) cov.dumpTo(logDir),
    )

    progressBar.summary
  }

  /** parse test */
  def parseTest(
    paths: Iterable[String] = Nil,
    log: Boolean = false,
    useProgress: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
  ): Summary = {
    // extract tests from paths
    val tests: List[Test] = getTests(paths.toList)

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "parse",
      targetTests = targetTests,
      removed = removed,
      useProgress = useProgress,
    )

    // run tests with logging
    logForTests(
      name = "parse",
      progressBar = progressBar,
      log = log,
    )(
      // check parsing result with its corresponding code
      check = test =>
        val filename = test.path
        val ast = parseFile(filename)
        val newAst = parse(ast.toString(grammar = Some(cfg.grammar)))
        if (ast != newAst) throw UnexpectedParseResult,
    )

    progressBar.summary
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // parse ECMAScript code
  private lazy val scriptParser = cfg.scriptParser
  private def parse(code: String): Ast = scriptParser.from(code)
  private def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // eval ECMAScript code
  private def evalFile(
    filename: String,
    log: Boolean = false,
    timeLimit: Option[Int] = None,
  ): State =
    val ast = loadTest(filename)
    val code = ast.toString(grammar = Some(cfg.grammar)).trim
    val st = Initialize(cfg, code, Some(ast), Some(filename))
    Interpreter(
      st = st,
      log = log,
      logDir = TEST262TEST_LOG_DIR,
      timeLimit = timeLimit,
    )

  // logging mode for tests
  private def logForTests(
    name: String,
    progressBar: ProgressBar[Test],
    postSummary: => String = "",
    log: Boolean = false,
  )(
    check: Test => Unit,
    postJob: String => Unit = _ => {},
  ): Unit =
    val summary: Summary = progressBar.summary
    val logDir = s"$TEST262TEST_LOG_DIR/$name-$dateStr"

    // setting for logging
    if (log)
      mkdir(logDir)
      dumpFile(spec.versionString, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")

    // run tests
    for (test <- progressBar) check(test)

    // logging after tests
    if (log)
      summary.dumpTo(logDir)
      val summaryStr =
        if (postSummary.isEmpty) s"$summary"
        else s"$summary$LINE_SEP$postSummary"
      dumpFile(s"Test262 $name test summary", summaryStr, s"$logDir/summary")
}
object Test262 extends Git(TEST262_DIR)
