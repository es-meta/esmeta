package esmeta.test262

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.{NotSupported, InvalidExit, UnexpectedParseResult}
import esmeta.error.NotSupported.{*, given}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.interpreter.Interpreter
import esmeta.parser.{AstFrom, ESParser}
import esmeta.peval.{ES_PE_TARGET}
import esmeta.state.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.util.concurrent.TimeoutException

// todo sort imports
import esmeta.peval.ESPartialEvaluator
import esmeta.peval.util.{AstHelper, Test262PEvalPolicy}

import java.nio.file.FileAlreadyExistsException

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
    loadTest(filename, Test(filename).includes)._1

  /** load test262 */
  def loadTestAndAst(filename: String): (Ast, Ast) =
    loadTest(filename, Test(filename).includes)

  /** load test262 with harness files */
  def loadTest(filename: String, includes: List[String]): (Ast, Ast) =
    // load harness
    val harnessStmts = includes.foldLeft(basicHarness)(_ ++ getHarness(_)())

    // merge with harnesses
    val fileAst = parseFile(filename)
    val stmts = flattenStmt(fileAst)
    (mergeStmt(harnessStmts ++ stmts), fileAst)

  /** get tests */
  def getTests(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    log: Boolean = false,
  ): List[Test] =
    if (log) println("- Extracting tests of Test262 tests...")
    Test.fromDirs(paths.getOrElse(List(TEST262_TEST_DIR)), features)

  /** get tests */
  def getProgressBar(
    name: String,
    targetTests: List[Test],
    log: Boolean = false,
    pw: PrintWriter,
    removed: Iterable[(Test, ReasonPath[String])] = Nil,
    useProgress: Boolean = false,
    useErrorHandler: Boolean = true,
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  ): ProgressBar[Test] = ProgressBar(
    msg = s"Run Test262 $name tests",
    iterable = targetTests,
    notSupported = removed,
    getName = (test, _) => test.relName,
    errorHandler = (e, summary, name) =>
      if (useErrorHandler) e match
        case NotSupported(reasons) =>
          summary.notSupported.add(name, reasons)
        case _: TimeoutException =>
          if (log)
            pw.println(s"[TIMEOUT] $name")
            pw.flush
          summary.timeout.add(name)
        case e: Throwable =>
          if (log)
            pw.println(s"[FAIL   ] $name")
            pw.println(e.getStackTrace.mkString(LINE_SEP))
            pw.flush
          summary.fail.add(name, getMessage(e))
      else throw e,
    verbose = useProgress,
    concurrent = concurrent,
  )

  /** interpreter test */
  def evalTest(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    log: Boolean = false,
    detail: Boolean = false,
    useProgress: Boolean = false,
    useCoverage: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
    peval: Test262PEvalPolicy = Test262PEvalPolicy.DEFAULT,
  ): Summary = {

    // log directory
    var THIS_TEST_LOG_DIR = {
      val firstPath = s"$TEST262TEST_LOG_DIR/eval-$dateStr"
      if (!pathExists(firstPath)) then firstPath
      else
        LazyList
          .range(1, 5000)
          .map(_.toString)
          .find(path =>
            !pathExists(s"$TEST262TEST_LOG_DIR/eval-$dateStr-$path"),
          )
          .getOrElse(
            throw FileAlreadyExistsException(firstPath),
          )
    }

    // extract tests from paths
    val tests: List[Test] = getTests(paths, features)

    // use error handler for multiple targets
    val multiple = tests.length > 1

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // open log file
    val logPW = getPrintWriter(s"$THIS_TEST_LOG_DIR/log")

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "eval",
      targetTests = targetTests,
      log = log,
      pw = logPW,
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
      pw = logPW,
      postSummary = if (useCoverage) cov.toString else "",
      log = log && multiple,
      logDir = THIS_TEST_LOG_DIR,
    )(
      // check final execution status of each Test262 test
      check = test =>
        val filename = test.path
        val st =
          if (!useCoverage)
            evalFile(
              filename,
              log && !multiple,
              detail,
              Some(logPW),
              timeLimit,
              peval,
              logDir = THIS_TEST_LOG_DIR,
            )
          else cov.run(filename)
        val returnValue = st(GLOBAL_RESULT)
        if (returnValue != Undef) throw InvalidExit(returnValue)
        st(GLOBAL_RESULT_ITER_COUNT)
      ,
      // dump coverage
      logDir => if (useCoverage) cov.dumpTo(logDir),
      successSummary = iterCounts => {
        val (totalIterCount, meanIterCount) =
          // if this line fails - it means st(GLOBAL_RESULT_ITER_COUNT above is not `Math`. similar to InvalidExit)
          val iters = iterCounts.map(_.asMath)
          (iters.sum, iters.sum / iters.size)
        s"total iter count: $totalIterCount${LINE_SEP}mean iter count:$meanIterCount${LINE_SEP}"
      },
    )

    // close log file
    logPW.close()

    if (log) then
      for {
        f <- cfg.funcs
        if f.irFunc.trueName.isDefined
      } {
        val pevalPw = getPrintWriter(
          s"$THIS_TEST_LOG_DIR/peval/${f.name}.ir",
        )
        pevalPw.println(f.irFunc)
        pevalPw.close
      }

    progressBar.summary
  }

  /** parse test */
  def parseTest(
    paths: Option[List[String]] = None,
    log: Boolean = false,
    useProgress: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  ): Summary = {
    // extract tests from paths
    val tests: List[Test] = getTests(paths)

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // open log file
    val logPW = getPrintWriter(s"$TEST262TEST_LOG_DIR/log")

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "parse",
      targetTests = targetTests,
      pw = logPW,
      removed = removed,
      useProgress = useProgress,
      concurrent = concurrent,
    )

    // run tests with logging
    logForTests(
      name = "parse",
      progressBar = progressBar,
      pw = logPW,
      log = log,
      logDir = s"$TEST262TEST_LOG_DIR/parse-$dateStr",
    )(
      // check parsing result with its corresponding code
      check = test =>
        val filename = test.path
        val ast = parseFile(filename)
        val newAst = parse(ast.toString(grammar = Some(cfg.grammar)))
        if (ast != newAst) throw UnexpectedParseResult,
    )

    // close log file
    logPW.close()

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
    detail: Boolean = false,
    logPW: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    peval: Test262PEvalPolicy = Test262PEvalPolicy.DEFAULT,
    logDir: String, // = s"$TEST262TEST_LOG_DIR",
  ): State =
    val (ast, fileAst) = loadTestAndAst(filename)
    val code = ast.toString(grammar = Some(cfg.grammar)).trim
    val chosenCfg =
      if (!peval.individual.shouldCompute) then cfg
      else /* peval.individual.shouldCompute */
        {
          val newCfg = ESPartialEvaluator.pevalThenConstructCFG(
            cfg,
            AstHelper
              .getPEvalTargetAsts(fileAst)
              .zipWithIndex
              .map((decl, idx) =>
                (
                  decl,
                  Some(s"${ES_PE_TARGET}PEvaled${idx + 2048}"),
                ),
              // Ad-hoc fix : add 2048 to idx to avoid name conflict
              ),
          )(None, None, None)
          if (peval.individual.shouldUse) then newCfg
          else cfg
        }
    val st = Initialize(chosenCfg, code, Some(ast), Some(filename))
    Interpreter(
      st = st,
      log = log,
      detail = detail,
      logPW = logPW,
      timeLimit = timeLimit,
    )

  // logging mode for tests
  private def logForTests[T, U](
    name: String,
    progressBar: ProgressBar[Test],
    pw: PrintWriter,
    postSummary: => String = "",
    log: Boolean = false,
    logDir: String,
  )(
    check: Test => U,
    postJob: String => Unit = _ => {},
    successSummary: Iterable[U] => String = (_: Iterable[U]) => "",
  ): Unit =
    val summary: Summary = progressBar.summary

    // setting for logging
    if (log)
      mkdir(logDir)
      dumpFile(spec.versionString, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")

    // run tests
    val iterCounts = for (test <- progressBar) yield check(test)

    // logging after tests
    if (log)
      summary.dumpTo(logDir)
      val summaryStr =
        if (postSummary.isEmpty) s"$summary"
        else s"$summary$LINE_SEP$postSummary"
      val additionalSummary = successSummary(iterCounts)
      dumpFile(
        s"Test262 $name test summary",
        s"${additionalSummary}${LINE_SEP}${summaryStr}",
        s"$logDir/summary",
      )

}
object Test262 extends Git(TEST262_DIR) {

  def getAllHarnesses(scriptParser: AstFrom) =
    val harnesses =
      for {
        file <- walkTree(s"$TEST262_DIR/harness")
        if file.isFile && jsFilter(file.getName)
        hs <- scriptParser.fromFile(file.toString).flattenStmt
      } yield hs
    harnesses.toList

  def pevalHarness = { ??? }
}
