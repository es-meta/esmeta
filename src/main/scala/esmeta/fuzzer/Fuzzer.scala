package esmeta.fuzzer

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.fuzzer.mutator.*
import esmeta.fuzzer.synthesizer.*
import esmeta.ty.util.TypeErrorCollector
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMeta, FUZZ_LOG_DIR, LINE_SEP}
import java.io.PrintWriter
import java.util.concurrent.{ConcurrentHashMap => CMMap, TimeoutException}
import scala.collection.mutable.{Map => MMap}
import scala.collection.parallel.CollectionConverters.*
import scala.util.*

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer {
  def apply(
    cfg: CFG,
    tyCheck: Boolean,
    log: Boolean = false, // logging mode on/off
    logInterval: Int = 600, // default is 600 s (10 m).
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no-debug
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    init: Option[String] = None, // initial pool directory path given by user
    kFs: Int = 0,
    cp: Boolean = false,
  ): Coverage = new Fuzzer(
    cfg,
    tyCheck,
    log,
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    duration,
    init,
    kFs,
    cp,
  ).result

  // debugging levels
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
}

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  tyCheck: Boolean,
  log: Boolean,
  logInterval: Int,
  debug: Int,
  stdOut: Boolean,
  timeLimit: Option[Int],
  trial: Option[Int],
  duration: Option[Int],
  init: Option[String],
  kFs: Int,
  cp: Boolean,
) {
  import Coverage.*, Fuzzer.*

  /** ECMAScript grammar */
  lazy val grammar = cfg.grammar

  /** script parser */
  lazy val scriptParser = cfg.scriptParser

  /** type error collector */
  lazy val collector: TypeErrorCollector = new TypeErrorCollector

  /** generated ECMAScript programs */
  lazy val result: Coverage = {
    startTime = System.currentTimeMillis
    if (log) {
      // start logging
      mkdir(logDir, remove = true)
      createSymLink(symlink, logDir, overwrite = true)
      dumpFile(ESMeta.currentVersion, s"$logDir/version")
      dumpFile(getSeed, s"$logDir/seed")
      genSummaryHeader
      genStatHeader(selector.names, selStatTsv)
      genStatHeader(mutator.names, mutStatTsv)
    }
    time(
      s"- initializing program pool with ${initPool.size} programs", {
        var i = 1
        for {
          (synthesizer, initCode) <- initPool
          rawStr = initCode.toString
          normalized <- optional(
            scriptParser.from(rawStr).toString(grammar = Some(grammar)),
          )
        } {
          debugging(f"[${synthesizer}:$i/${initPool.size}%-30s] $normalized")
          i += 1
          add(normalized, initCode)
        }
      },
    )
    println(s"- the initial program pool consists of ${pool.size} programs.")
    time(
      "- repeatedly trying to fuzz new programs to increase coverage", {
        if (log) {
          startInterval = System.currentTimeMillis
          logging
        }
        trial match
          case Some(count) => for (_ <- Range(0, count)) if (!timeout) fuzz
          case None        => while (!timeout) fuzz
      },
    )

    // finish logging
    if (log) {
      logging
      summaryTsv.close
      selStatTsv.close
      mutStatTsv.close
    }

    cov
  }

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz new programs to increase coverage */
  def fuzz: Unit = {
    iter += 1

    val startTime = System.currentTimeMillis
    debugging(("-" * 40) + f"  iter: $iter%10d  " + ("-" * 40))
    if (log) {
      val bound = logInterval * 1000
      if (interval > bound)
        if (debug == NO_DEBUG) logging else time("Logging", logging)
        startInterval += bound
    }
    val (selectorName, script, condView) = selector(pool, cov)
    val selectorInfo = selectorName + condView.map(" - " + _).getOrElse("")
    val code = script.code
    debugging(f"[$selectorInfo%-30s] $code")
    debugFlush

    val mutants: List[(Mutator.Result, CandInfo)] =
      val results = mutator(code, 100, condView.map((_, cov))).par
      results.map(result => (result, getCandInfo(result.code))).toList

    for ((res @ Mutator.Result(mutatorName, mutantCode), info) <- mutants)
      debugging(f"----- $mutatorName%-20s-----> ${res.code}")

      val result = add(res.code, info, mutantCode)
      update(selectorName, selectorStat, result)
      update(mutatorName, mutatorStat, result)

    val duration = Time(System.currentTimeMillis - startTime)
    debugging(s"iter/end: $iter - $duration")
  }

  /** the information about a candidate */
  case class CandInfo(
    visited: Boolean = false,
    invalid: Boolean = false,
    interp: Option[Try[Coverage.Interp]] = None,
  )

  /** get candidate information */
  def getCandInfo(code: String): CandInfo =
    if (!visited.add(code)) CandInfo(visited = true)
    else if (!ValidityChecker(code)) CandInfo(invalid = true)
    else CandInfo(interp = Some(Try(cov.run(code))))

  /** add new program */
  def add(code: String): Boolean =
    add(code, getCandInfo(code), Code.Simple(code))
  def add(code: String, codeObj: Code): Boolean =
    add(code, getCandInfo(code), codeObj)

  /** add mutant with precomputed info */
  def add(mutant: String, info: CandInfo, codeObj: Code): Boolean =
    handleResult(
      mutant,
      Try {
        if (info.visited) fail("ALREADY VISITED")
        if (info.invalid) fail("INVALID PROGRAM")
        val interp = info.interp.get match
          case Success(v) => v
          case Failure(e) => throw e
        val finalState = interp.result
        val supported = interp.supported
        val script = toScript(codeObj, supported)
        if (tyCheck) collector.add(mutant, finalState.typeErrors)
        val (_, updated, covered) = cov.check(script, interp)
        if (!updated) fail("NO UPDATE")
        (covered, supported)
      },
    )

  /** handle add result */
  def handleResult(code: String, result: Try[(Boolean, Boolean)]): Boolean = {
    debugging(f" ${"COVERAGE RESULT"}%30s: ", newline = false)
    val pass = result match
      case Success(covered, supported) =>
        val msg = if supported then "" else "NOT SUPPORTED"
        debugging(passMsg(msg))
        covered
      case Failure(e: TimeoutException) =>
        debugging(failMsg("TIMEOUT"))
        false
      case Failure(e: ESMetaError) =>
        debugging(failMsg("ESMETA ERROR"))
        esmetaErrors += e -> (esmetaErrors.getOrElse(e, Set()) + code)
        false
      case Failure(e) =>
        e.getMessage match
          case "ALREADY VISITED" | "INVALID PROGRAM" if debug == PARTIAL =>
            debugClean
          case msg => debugging(failMsg(msg))
        false
    debugFlush
    pass
  }

  // a pass-or-fail counter
  case class Counter(pass: Int = 0, fail: Int = 0)
  def update[T](t: T, map: MMap[T, Counter], pass: Boolean): Unit =
    val Counter(p, f) = map.getOrElse(t, Counter())
    val updated = if (pass) Counter(p + 1, f) else Counter(p, f + 1)
    map += t -> updated

  /** coverage */
  val cov: Coverage = Coverage(cfg, tyCheck, kFs, cp, timeLimit)

  /** target selector */
  val selector: TargetSelector = WeightedSelector(
    RandomSelector -> 2,
    BranchSelector -> 8,
  )

  /** selector stat */
  val selectorStat: MMap[String, Counter] = MMap()

  given CFG = cfg

  /** mutator */
  val mutator: Mutator = WeightedMutator(
    NearestMutator() -> 6,
    RandomMutator() -> 3,
    StatementInserter() -> 1,
    Remover() -> 1,
    SpecStringMutator() -> 1,
  )

  /** mutator stat */
  val mutatorStat: MMap[String, Counter] = MMap()

  /** initial pool */
  val initPool =
    val inits = init.map(dir => walkTree(dir).filter(_.isFile).toList.sorted)
    inits match
      case Some(files) =>
        files.map { file =>
          val sourceText = readFile(file.getPath).replace(USE_STRICT, "")
          "GivenByUser" -> Code.Simple(sourceText)
        }
      case None =>
        // FIXME: use only builtin init pool for experiment
        // val simpleSyn = SimpleSynthesizer(grammar)
        val builtinSyn = BuiltinSynthesizer(cfg.spec.algorithms)
        // simpleSyn.initPool.map(code => simpleSyn.name -> code) ++
        builtinSyn.initPool.map(code => builtinSyn.name -> code)

  lazy val logDir: String = s"$FUZZ_LOG_DIR/fuzz-$dateStr"
  lazy val symlink: String = s"$FUZZ_LOG_DIR/recent"

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // current iteration count
  private var iter: Int = 0

  // current id
  private var idCounter: Long = 0
  private def nextId: Long = { val id = idCounter; idCounter += 1; id }

  // evaluation start time
  private var startTime: Long = 0L
  private def elapsed: Long = System.currentTimeMillis - startTime
  private def timeout = duration.fold(false)(_ * 1000 < elapsed)
  private var startInterval: Long = 0L
  private def interval: Long = System.currentTimeMillis - startInterval

  // conversion from code string to `Script` object
  private def toScript(code: Code, supported: Boolean): Script =
    Script(code, s"$nextId.js", supported)

  // check if the added code is visited (thread-safe)
  // NOTE: Scala has no concurrent Set impl so used Java's ConcurrentHashMap
  private val visited: java.util.Set[String] = CMMap.newKeySet()

  // indicating that add failed
  private def fail(msg: String) = throw Exception(msg)

  // ESMeta errors collected during fuzzing
  private val esmetaErrors: MMap[ESMetaError, Set[String]] = MMap()

  // debugging
  private var debugMsg = ""
  private def debugging(
    msg: String,
    newline: Boolean = true,
  ): Unit = if (debug == ALL) {
    if (newline) println(msg) else print(msg)
  } else if (debug > NO_DEBUG) {
    debugMsg += msg
    if (newline) debugMsg += LINE_SEP
  }
  private def debugClean: Unit = debugMsg = ""
  private def debugFlush: Unit = { print(debugMsg); debugClean }

  // generate headers
  private def genSummaryHeader =
    var header = Vector(
      "iter(#)",
      "time(ms)",
      "time(h:m:s)",
      "program(#)",
      "minimal(#)",
      "node(#)",
      "branch(#)",
    )
    if (kFs > 0) header ++= Vector(s"sens-node(#)", s"sens-branch(#)")
    header ++= Vector("target-conds(#)")
    if (kFs > 0) header ++= Vector(s"sens-target-conds(#)")
    addRow(header)
  private def genStatHeader(keys: List[String], nf: PrintWriter) =
    var header1 = Vector("iter(#)")
    var header2 = Vector("-")
    keys.foreach(k => {
      header1 ++= Vector(k, "-", "-", "-")
      header2 ++= Vector("pass", "fail", "total", "ratio")
    })
    addRow(header1, nf)
    addRow(header2, nf)

  // dump selector and mutator stat
  private def dumpStat(
    keys: List[String],
    stat: MMap[String, Counter],
    tsv: PrintWriter,
  ): Unit =
    var row = Vector[Any](iter)
    keys.foreach(k => {
      val Counter(pass, fail) = stat.getOrElse(k, Counter())
      val total = pass + fail
      val ratio = optional((pass * 10000) / total / 100.0).getOrElse(0.0)
      row ++= Vector(pass, fail, total, s"$ratio%")
    })
    addRow(row, tsv)

  // logging
  private def logging: Unit =
    val n = cov.nodeCov
    val b = cov.branchCov
    val e = elapsed
    val t = Time(e).simpleString
    val nv = cov.nodeViewCov
    val bv = cov.branchViewCov
    val tc = cov.targetCondViews.size
    val tcv = cov.targetCondViews.map(_._2.size).fold(0)(_ + _)
    var row = Vector(iter, e, t, visited.size, pool.size, n, b)
    if (kFs > 0) row ++= Vector(nv, bv)
    row ++= Vector(tc)
    if (kFs > 0) row ++= Vector(tcv)
    addRow(row)
    // dump coverage
    cov.dumpToWithDetail(logDir, withMsg = (debug == ALL))
    dumpStat(selector.names, selectorStat, selStatTsv)
    dumpStat(mutator.names, mutatorStat, mutStatTsv)
    // dump spec type error
    if (tyCheck) collector.dumpTo(logDir)
    // dump ESMeta errors
    dumpFile(
      name = "found ESMeta errors",
      data = esmetaErrors.toVector
        .sortBy(_._1.errMsg)
        .map { (e, codes) =>
          s"[InterpreterError] ${e.errMsg}" + LINE_SEP +
          s"thrown when executing:" + LINE_SEP +
          codes.toVector.sorted.map(code => s"- $code").mkString(LINE_SEP)
        }
        .mkString(LINE_SEP + LINE_SEP),
      filename = s"$logDir/esmeta-errors",
    )

  private def addRow(data: Iterable[Any], nf: PrintWriter = summaryTsv): Unit =
    val row = data.mkString("\t")
    if (stdOut) println(row)
    nf.println(row)
    nf.flush

  private lazy val summaryTsv: PrintWriter =
    getPrintWriter(s"$logDir/summary.tsv")
  private lazy val selStatTsv: PrintWriter =
    getPrintWriter(s"$logDir/selector-stat.tsv")
  private lazy val mutStatTsv: PrintWriter =
    getPrintWriter(s"$logDir/mutation-stat.tsv")
}
