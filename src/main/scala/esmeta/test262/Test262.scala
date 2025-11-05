package esmeta.test262

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported, InvalidExit, UnexpectedParseResult}
import esmeta.error.NotSupported.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.interpreter.Interpreter
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.ty.util.TypeErrorCollector
import esmeta.test262.util.*
import esmeta.util.{ConcurrentPolicy as CP, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.util.concurrent.TimeoutException

/** data in Test262 */
class Test262(
  val version: Test262.Version,
  val cfg: CFG,
  val withYet: Boolean = false,
) {

  type Filename = String
  type CodeVec = (Vector[Ast], String)
  type Code = (Ast, String)
  extension (pair: CodeVec) {
    def +(that: CodeVec): CodeVec =
      val (lstmts, lstr) = pair
      val (rstmts, rstr) = that
      (lstmts ++ rstmts, lstr + rstr)
    def toCode: Code =
      val (stmts, str) = pair
      (mergeStmt(stmts), str)
  }
  extension (pair: Code) {
    def toCodeVec: CodeVec =
      val (ast, str) = pair
      (ast.flattenStmt, str)
  }

  /** cache for parsing results for necessary harness files */
  lazy val getHarness = cached[Filename, CodeVec] { name =>
    parseFile(s"$TEST262_DIR/harness/$name").toCodeVec
  }

  /** test262 filter */
  lazy val testFilter: TestFilter = TestFilter(cfg.spec)

  /** all Test262 tests */
  lazy val allTests: List[Test] = Test.fromDir(TEST262_TEST_DIR)

  /** test262 test configuration */
  lazy val (allTargetTests, allRemoved) = testFilter(allTests, withYet)

  /** basic harness files */
  lazy val basicHarness: CodeVec =
    getHarness("assert.js") + getHarness("sta.js")

  /** specification */
  val spec = cfg.spec

  /** load test262 */
  def loadTest(filename: String): Code =
    loadTest(filename, Test(filename).includes)

  /** load test262 with harness files */
  def loadTest(filename: String, includes: List[String]): Code =
    // load harness
    val harness = includes.foldLeft(basicHarness)(_ + getHarness(_))
    // merge with harnesses
    (harness + parseFile(filename).toCodeVec).toCode

  /** get tests */
  def getTests(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    log: Boolean = false,
  ): List[Test] =
    if (log) println("- Extracting tests of Test262 tests...")
    Test.fromDirs(paths.getOrElse(List(TEST262_TEST_DIR)), features)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // parse ECMAScript code
  private lazy val scriptParser = cfg.scriptParser
  private def parse(code: String): Code =
    scriptParser.fromWithCode(code)
  private def parseFile(filename: String): Code =
    scriptParser.fromFileWithCode(filename)

}
object Test262 extends Git(TEST262_DIR) {

  /** interpreter test */
  def evalTest(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    tyCheck: Boolean = false,
    log: Boolean = false,
    detail: Boolean = false,
    useProgress: Boolean = false,
    useCoverage: Boolean = false,
    kFs: Int = 0,
    cp: Boolean = false,
    allTests: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  )(using test262: Test262): Summary = {

    lazy val cov = Coverage(
      cfg = test262.cfg,
      timeLimit = timeLimit,
      kFs = kFs,
      cp = cp,
      all = allTests,
      isTargetNode = (_, st) => !Test262.inHarness(st),
      isTargetBranch = (_, st) => !Test262.inHarness(st),
    )

    // pre-jobs
    val tests: List[Test] = test262.getTests(paths, features)
    val (targetTests, removed) = test262.testFilter(tests, test262.withYet)
    val multiple = targetTests.length > 1

    // dump test id to path for -test262test:all-tests
    if (allTests)
      dumpJson(
        targetTests.map(_.relName).zipWithIndex.map(_.swap).toMap,
        s"$TEST262TEST_LOG_DIR/test262IdToTest262.json",
      )
    val name = "eval"
    val logDir = s"$TEST262TEST_LOG_DIR/$name-$dateStr"
    val symlink = s"$TEST262TEST_LOG_DIR/recent"

    // setting for logging
    if (log)
      mkdir(logDir)
      dumpFile(test262.spec.versionString, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")

    new Test262Runner(
      msg = s"Run Test262 $name tests",
      targets = targetTests,
      notSupported = removed,
      concurrent = concurrent,
      showProgressBar = useProgress,
      detail = detail,
    ) {

      override lazy val getName = (test, _) => test.relName

      val logPW = getPrintWriter(s"$TEST262TEST_LOG_DIR/log")

      /** type error collector */
      lazy val collector: TypeErrorCollector = new TypeErrorCollector

      override def errorHandler(
        error: Throwable,
        summary: Summary,
        name: String,
      ): Unit =
        if (multiple) error match
          case NotSupported(reasons) =>
            summary.notSupported.add(name, reasons)
          case _: TimeoutException =>
            if (log)
              logPW.println(s"[TIMEOUT] $name")
              logPW.flush
            summary.timeout.add(name)
          case e: Throwable =>
            if (log)
              logPW.println(s"[FAIL   ] $name")
              logPW.println(e.getStackTrace.mkString(LINE_SEP))
              logPW.flush
            summary.fail.add(name, getMessage(e))
        else throw error

      override def postJob: Summary = {
        val summary = super.postJob

        val postSummary =
          if (useCoverage) cov.toString else ""

        // logging after tests
        if (log)
          summary.dumpTo(logDir)
          val summaryStr =
            if (postSummary.isEmpty) s"$summary"
            else s"$summary$LINE_SEP$postSummary"
          dumpFile(
            s"Test262 $name test summary",
            summaryStr,
            s"$logDir/summary",
          )

          // dump type errors (dump names only when there are multiple tests)
          if (tyCheck) collector.dumpTo(logDir, withNames = multiple)
          // dump coverage
          if (useCoverage) cov.dumpTo(logDir)

        if exists(logDir) then createSymLink(symlink, logDir, overwrite = true)
        logPW.close()
        summary
      }

      override def runTest(test: Test): Unit = {
        val filename = test.path
        val st = useCoverage match {
          case false =>
            evalFile(
              filename,
              tyCheck,
              log && !multiple,
              this.detail,
              Some(logPW),
              timeLimit,
            )
          case true =>
            val (ast, code) = test262.loadTest(filename)
            cov.runAndCheck(Script(code, filename), ast)._1
        }
        if (tyCheck) collector.add(filename, st.typeErrors)
        val returnValue = st(GLOBAL_RESULT)
        if (returnValue != Undef) throw InvalidExit(returnValue)
      }
    }.result
  }

  /** parse test */
  def parseTest(
    log: Boolean = false,
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  )(using test262: Test262): Summary = {
    // extract tests from paths
    val tests: List[Test] = test262.getTests(None)

    // get target tests and removed tests
    val (targetTests, removed) = test262.testFilter(tests, test262.withYet)

    // open log file
    val logPW = getPrintWriter(s"$TEST262TEST_LOG_DIR/log")

    new Test262Runner(
      msg = s"Parse Test262 tests",
      targets = targetTests,
      notSupported = removed,
      concurrent = concurrent,
      showProgressBar = false,
      detail = false,
    ) {

      override lazy val getName = (test, _) => test.relName

      private lazy val scriptParser = test262.cfg.scriptParser
      private def parse(code: String): Test262#Code =
        scriptParser.fromWithCode(code)
      private def parseFile(filename: String): Test262#Code =
        scriptParser.fromFileWithCode(filename)

      def runTest(test: Test): Unit = {
        val filename = test.path
        val (ast, _) = parseFile(filename)
        val (newAst, _) = parse(
          ast.toString(grammar = Some(test262.cfg.grammar)),
        )
        if (ast != newAst) throw UnexpectedParseResult
      }
    }.result
  }

  /** check whether program point is in harness */
  private def inHarness(st: State): Boolean = (for {
    ast <- st.context.astOpt.orElse(
      st.callStack.view.flatMap(_.context.astOpt).headOption,
    )
    loc <- ast.loc
    filename <- loc.filename
    if filename.contains("test262/harness")
  } yield true).getOrElse(false)

  /* eval ECMAScript code */
  private def evalFile(
    filename: String,
    tyCheck: Boolean,
    log: Boolean = false,
    detail: Boolean = false,
    logPW: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
  )(using test262: Test262): State =
    val (ast, code) = test262.loadTest(filename)
    val st = test262.cfg.init.from(code, ast)
    Interpreter(
      st = st,
      tyCheck = tyCheck,
      log = log,
      detail = detail,
      logPW = logPW,
      timeLimit = timeLimit,
    )

}
