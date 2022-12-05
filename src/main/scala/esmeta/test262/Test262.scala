package esmeta.test262

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported, InvalidExit, UnexpectedParseResult}
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

  /** all Test262 tests */
  lazy val allTests: List[MetaData] = MetaData.fromDir(TEST262_TEST_DIR)

  /** test262 test configuration */
  lazy val allTestFilter: TestFilter = TestFilter(cfg.spec, allTests, withYet)

  /** configuration summary for applicable tests */
  lazy val config: ConfigSummary = allTestFilter.summary

  /** basic harness files */
  lazy val basicHarness = getHarness("assert.js")() ++ getHarness("sta.js")()

  /** specification */
  val spec = cfg.spec

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

  /** get data list */
  def getDataList(
    paths: List[String] = Nil,
    log: Boolean = false,
  ): List[MetaData] =
    if (log) println("- Extracting metadata of Test262 tests...")
    MetaData.fromDirs(paths match
      case Nil   => List(TEST262_TEST_DIR)
      case paths => paths,
    )

  /** get tests */
  def getTests(
    name: String,
    dataList: List[MetaData],
    useProgress: Boolean = false,
    useErrorHandler: Boolean = true,
  ): (ProgressBar[NormalConfig], Map[String, List[MetaData]]) =
    val testFilter = TestFilter(cfg.spec, dataList, withYet)
    (
      ProgressBar(
        msg = s"Run Test262 $name tests",
        iterable = testFilter.summary.normal,
        getName = (test, _) =>
          val name = test.name
          val absPath = getAbsPath(name)
          if (absPath.startsWith(TEST262_TEST_DIR))
            absPath.drop(TEST262_TEST_DIR.length + 1)
          else name
        ,
        errorHandler = (e, summary, name) =>
          if (useErrorHandler) e match
            case NotSupported(msg)   => summary.yets += s"$name - $msg"
            case _: TimeoutException => summary.timeouts += name
            case e: Throwable => summary.fails += s"$name - ${e.getMessage}"
          else throw e,
        verbose = useProgress,
      ),
      testFilter.removedTests,
    )

  /** interpreter test */
  def evalTest(
    paths: Iterable[String] = Nil,
    log: Boolean = false,
    useProgress: Boolean = false,
    useCoverage: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
  ): Summary = {
    // get metadata list
    val dataList: List[MetaData] = getDataList(paths.toList)

    // use error handler for multiple targets
    val multiple = dataList.length > 1

    // get all applicable tests with progress bar
    val (tests, filtered) = getTests(
      name = "eval",
      dataList = dataList,
      useProgress = useProgress,
      useErrorHandler = multiple,
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
      tests = tests,
      filtered = filtered,
      postSummary = if (useCoverage) cov.toString else "",
      log = log && multiple,
    )(
      // check final execution status of each Test262 test
      check = test =>
        val filename = test.name
        val st =
          if (!useCoverage) evalFile(filename, log && !multiple, timeLimit)
          else cov.run(filename)
        val returnValue = st(GLOBAL_RESULT)
        if (returnValue != Undef) throw InvalidExit(returnValue)
      ,
      // dump coverage
      logDir => if (useCoverage) cov.dumpTo(logDir),
    )

    tests.summary
  }

  /** parse test */
  def parseTest(
    paths: Iterable[String] = Nil,
    log: Boolean = false,
    useProgress: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
  ): Summary = {
    // get metadata list
    val dataList: List[MetaData] = getDataList(paths.toList)

    // get all applicable tests with progress bar
    val (tests, filtered) = getTests(
      name = "parse",
      dataList = dataList,
      useProgress = useProgress,
    )

    // run tests with logging
    logForTests(
      name = "parse",
      tests = tests,
      filtered = filtered,
      log = log,
    )(
      // check parsing result with its corresponding code
      check = test =>
        val filename = test.name
        val ast = parseFile(filename)
        val newAst = parse(ast.toString(grammar = Some(cfg.grammar)))
        if (ast != newAst) throw UnexpectedParseResult,
    )

    tests.summary
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
  ): State = eval(loadTest(filename), log, timeLimit)
  private def eval(
    script: Ast,
    log: Boolean = false,
    timeLimit: Option[Int] = None,
  ): State =
    val code = script.toString(grammar = Some(cfg.grammar)).trim
    val st = Initialize(cfg, code, Some(script))
    Interpreter(
      st = st,
      log = log,
      logDir = TEST262TEST_LOG_DIR,
      timeLimit = timeLimit,
    )

  // logging mode for tests
  private def logForTests(
    name: String,
    tests: ProgressBar[NormalConfig],
    filtered: Map[String, List[MetaData]],
    postSummary: => String = "",
    log: Boolean = false,
  )(
    check: NormalConfig => Unit,
    postJob: String => Unit = _ => {},
  ): Unit =
    val summary = tests.summary
    val logDir = s"$TEST262TEST_LOG_DIR/$name-$dateStr"

    // setting for logging
    if (log)
      println(s"- Logging to $logDir...")
      mkdir(logDir)
      dumpFile(spec.versionString, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")
      summary.timeouts.setPath(s"$logDir/timeout.log")
      summary.yets.setPath(s"$logDir/yet.log")
      summary.fails.setPath(s"$logDir/fail.log")
      summary.passes.setPath(s"$logDir/pass.log")

    // run tests
    for (test <- tests) check(test)

    // logging after tests
    if (log)
      summary.close
      val filtered_total = filtered.foldLeft(0)(_ + _._2.length)
      val summaryStr =
        s"- total: ${summary.total + filtered_total}$LINE_SEP" +
        s"- filtered: ${filtered_total}$LINE_SEP" +
        filtered.foldLeft("") {
          case (acc, (s, i)) => acc + s"  - $s: ${i.length}$LINE_SEP"
        }
        + (if (postSummary.isEmpty) s"$summary$LINE_SEP"
           else
             s"$summary$LINE_SEP$postSummary$LINE_SEP")
      dumpFile(s"Test262 $name test summary", summaryStr, s"$logDir/summary")
}
object Test262 extends Git(TEST262_DIR)
