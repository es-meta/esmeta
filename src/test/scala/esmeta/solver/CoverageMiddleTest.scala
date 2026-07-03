package esmeta.solver

import esmeta.cfg.{Branch, CFG, Call, Func}
import esmeta.es.util.Coverage
import esmeta.es.util.Coverage.Cond
import esmeta.ir.{EBool, EClo, ICall}
import esmeta.phase.Solve
import esmeta.ty.ValueTy
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMetaTest, SOLVER_LOG_DIR, BASE_DIR}
import io.circe.*, io.circe.generic.semiauto.*
import scala.collection.mutable.{Set => MSet, Queue}
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, ExecutionContext}
import java.util.concurrent.{
  Callable,
  ExecutorCompletionService,
  Executors,
  ThreadFactory,
  TimeUnit,
  TimeoutException,
}

class CoverageMiddleTest extends SolverTest {
  val name = "solverCovTest"

  lazy val cfg = ESMetaTest.cfg

  // -------------------------------------------------------------------------
  // XXX: remove later
  // -------------------------------------------------------------------------
  private case class ExpectedEntry(branch: Int, side: String, js: String)
  private case class ExpectedData(
    fingerprint: String,
    expected: List[ExpectedEntry],
  )
  private given Decoder[ExpectedEntry] = deriveDecoder
  private given Decoder[ExpectedData] = deriveDecoder

  private lazy val expectedInjection: Map[(Int, Boolean), String] =
    val file = s"$BASE_DIR/src/test/resources/expected.json"
    if (!exists(file)) Map.empty
    else
      val data = readJson[ExpectedData](file)
      if (data.fingerprint != cfg.fingerprint)
        println(
          "  [WARN] expected.json fingerprint mismatch with current CFG; " +
          "branch ids may be stale " +
          s"(json ${data.fingerprint.take(12)}..., " +
          s"cfg ${cfg.fingerprint.take(12)}...)",
        )
      data.expected.map(e => (e.branch, e.side == "T") -> e.js).toMap
  // -------------------------------------------------------------------------

  /** coverage-style branch filter: exclude compiler boilerplate */
  private def isTargetBranch(b: Branch): Boolean =
    !b.isFiltered && (b.cond match
      case EBool(_) => false
      case _        => true
    )

  /** static callees: resolve EClo targets in Call nodes */
  private def directCallees(f: Func): Set[Func] =
    f.nodes.collect { case c: Call => c.callInst }.flatMap {
      case ICall(_, EClo(name, _), _) => cfg.fnameMap.get(name)
      case _                          => None
    }

  /** transitive closure of callees from root functions */
  private def reachableFuncs(roots: List[Func]): Set[Func] =
    val visited = MSet.from(roots.map(_.id))
    val queue = Queue.from(roots)
    while (queue.nonEmpty)
      for
        callee <- directCallees(queue.dequeue())
        if visited.add(callee.id)
      do queue.enqueue(callee)
    visited.flatMap(cfg.funcMap.get).toSet

  private def targetBranches(fs: Iterable[Func]): List[Branch] =
    fs.flatMap(_.nodes.collect { case b: Branch if isTargetBranch(b) => b })
      .toList

  def init: Unit = {
    given CFG = cfg
    val cov = Coverage(cfg, timeLimit = Some(10))
    val solveTimeLimit = 10
    val solveTimeout = Duration(solveTimeLimit, "seconds")
    val allBuiltins = cfg.funcs.filter(_.isBuiltin).sortBy(_.name)

    val runner = SymInterp(
      cfg,
      timeLimit = Some(solveTimeLimit),
    )
    import runner.tyChecker.{cfg => _, *}, AbsState.given

    val nThreads = Runtime.getRuntime.availableProcessors
    val pool = Executors.newFixedThreadPool(
      nThreads,
      new ThreadFactory {
        private var nextId = 0
        def newThread(r: Runnable): Thread = {
          nextId += 1
          val t = new Thread(r, s"builtin-branch-test-$nextId")
          t.setDaemon(true)
          t
        }
      },
    )
    given ExecutionContext = ExecutionContext.fromExecutor(pool)
    val builtins = {
      val futures = allBuiltins.map { f =>
        Future {
          val isSupported = Solver.funcAccessExpr(f).exists { js =>
            try { cov.run(js + ".call();").supported }
            catch { case _: Throwable => false }
          }
          if (isSupported) Some(f) else None
        }
      }
      futures.flatMap(Await.result(_, Duration.Inf))
    }
    val reachableByBuiltin: List[(Func, Set[Func])] = {
      val futures = builtins.map { f =>
        Future(f -> reachableFuncs(List(f)))
      }
      futures.map(Await.result(_, Duration.Inf)).toList
    }
    val builtinEntries = builtins.toSet
    val reachableModelingAOs = reachableByBuiltin.flatMap(_._2).toSet
    val builtinEntryAONames = builtinEntries.map(_.name).toList.sorted
    val nonEntryAONames =
      (reachableModelingAOs -- builtinEntries).map(_.name).toList.sorted
    val modelingAOCount = reachableModelingAOs.size

    // collect unique branches; entry selection via reverse call graph
    // (matches `sbt run solve` so miss cases are reproducible)
    val accessibleBuiltins = builtins.toSet
    val funcEntryCache = collection.mutable.Map[Int, List[Func]]()
    def entriesFor(b: Branch): List[Func] =
      funcEntryCache.getOrElseUpdate(
        cfg.funcOf(b).id,
        SymInterp.sortedEntries(b).filter(accessibleBuiltins.contains),
      )
    val targetBranchEntries: List[(Func, Branch)] = {
      val seen = MSet[Int]()
      val buf = List.newBuilder[(Func, Branch)]
      for {
        (_, reachable) <- reachableByBuiltin
        b <- targetBranches(reachable).sortBy(_.id)
        if seen.add(b.id)
        entries = entriesFor(b)
        if entries.nonEmpty
      } buf += (entries.head -> b)
      buf.result()
    }
    val targets: List[(Func, Cond)] =
      targetBranchEntries.flatMap { (f, b) =>
        List(f -> Cond(b, true), f -> Cond(b, false))
      }

    check("builtin branches: solve and verify") {
      case class BranchResult(
        fname: String,
        targetName: String,
        targetCfg: String,
        bid: Int,
        side: Boolean,
        status: String,
        js: Option[String],
        attempts: Int,
        elapsed: Long,
        conds: Option[List[Cond]],
        calls: Option[List[Call]],
        saturated: Option[Map[Sym, ValueTy]],
      )

      def sideString(side: Boolean): String = if (side) "T" else "F"

      // human-readable label for a case: entry func, or "entry -> target"
      def caseLabel(r: BranchResult): String =
        if (r.fname == r.targetName) r.fname
        else s"${r.fname} -> ${r.targetName}"

      println(
        s"  Solving ${targets.size} branch sides from " +
        s"${targetBranchEntries.size} branches with $nThreads threads " +
        s"(timeout: $solveTimeout per side)...",
      )

      // per-case detail, written into one file per (branch, taken side)
      def dumpCase(out: String => Unit, r: BranchResult): Unit =
        out(stringify(r, "    "))

      given Rule[BranchResult] = (app, r) => {
        app >> f"[${r.status.toUpperCase}] ${caseLabel(r)}  "
        app >> f"Branch[${r.bid}]:${sideString(r.side)}"
        app >> f"  (${r.elapsed / 1e9}%.3fs)"
        app >> f"  attempts=${r.attempts}"
        app.wrap("", "") {
          app :> s"cfg: ${r.targetCfg}"
          for (js <- r.js) app :> s"js:  $js"
          // -------------------------------------------------------------------
          // XXX: remove later
          // -------------------------------------------------------------------
          for (js <- expectedInjection.get((r.bid, r.side)))
            app :> s"expected: $js"
          // -------------------------------------------------------------------
          for (ps <- r.conds) {
            val ss = ps.map(c => s"${c.branch.id}:${sideString(c.cond)}")
            app :> s"conds: [${ss.size}] ${ss.mkString(" <- ")}"
          }
          for (cs <- r.calls) {
            val ss = cs.map(c => s"${cfg.funcOf(c).name}:${c.id}")
            app :> s"calls: [${ss.size}] ${ss.mkString(" <- ")}"
          }
          given Rule[Map[Sym, ValueTy]] = AbsState.constrMapRule
          for (fs <- r.saturated if fs.nonEmpty)
            app :> s"saturated: " >> fs
        }
      }

      // write the detail log for a single result as soon as it is solved,
      // grouped by status so log/solver/<status>/ stays browsable
      def writeCaseLog(r: BranchResult): Unit = {
        val pw = getPrintWriter(
          s"$SOLVER_LOG_DIR/${r.status}/branch-${r.bid}-${sideString(r.side)}",
        )
        try dumpCase(s => pw.println(s), r)
        finally pw.close()
      }

      // clear previous run before streaming per-case logs into it
      mkdir(SOLVER_LOG_DIR, remove = true)

      val results = {
        def timeoutResult(
          f: Func,
          cond: Cond,
          attempts: Int = 0,
        ): BranchResult = {
          val b = cond.branch
          val targetFunc = cfg.funcOf(b)
          BranchResult(
            f.name,
            targetFunc.name,
            s"logs/cfg/func/${targetFunc.normalizedName}.cfg",
            b.id,
            cond.cond,
            "timeout",
            None,
            attempts,
            solveTimeout.toNanos,
            None,
            None,
            None,
          )
        }

        def solveTarget(f: Func, cond: Cond): BranchResult = {
          val t0 = System.nanoTime()
          val interp = runner(f, cond)
          def checkTimeout(): Unit =
            if (interp.timeout) throw TimeoutException("solver")
          def elapsedNanos: Long = System.nanoTime() - t0
          val b = cond.branch
          val targetFunc = cfg.funcOf(b)
          def result(
            status: String,
            js: Option[String],
            conf: Option[interp.Config],
            attempts: Int,
          ): BranchResult = {
            BranchResult(
              f.name,
              targetFunc.name,
              s"logs/cfg/func/${targetFunc.normalizedName}.cfg",
              b.id,
              cond.cond,
              status,
              js,
              attempts,
              elapsedNanos,
              conf.map(_.conds),
              conf.map(_.calls),
              conf.map(_.state.constrForSyms),
            )
          }
          case class Rejected(
            status: String,
            js: Option[String],
            conf: interp.Config,
          )
          def rejectedResult(
            rejected: Option[Rejected],
            attempts: Int,
          ): BranchResult = rejected
            .map(r => result(r.status, r.js, Some(r.conf), attempts))
            .getOrElse(result("unsolved", None, None, attempts))
          Thread.currentThread().setName {
            s"builtin-branch-test ${f.name} " +
            s"Branch[${b.id}]:${sideString(cond.cond)}"
          }

          def verifies(js: String): Boolean =
            checkTimeout()
            try {
              val interp = cov.run(js)
              checkTimeout()
              interp.touchedCondViews.keys.exists { cv =>
                cv.cond.branch.id == b.id && cv.cond.cond == cond.cond
              }
            } catch {
              case e: TimeoutException => throw e
              case _: Throwable        => false
            }

          var attempts = 0 // execution attempts
          val maxCandsPerPath = 64
          try {
            @scala.annotation.tailrec
            def retry(rejected: Option[Rejected]): BranchResult = {
              checkTimeout()
              interp.nextCandidate match {
                case Some(conf) =>
                  val cands = interp.reifyAll.take(maxCandsPerPath).toList
                  cands match {
                    case Nil if rejected.isEmpty =>
                      retry(Some(Rejected("fail-reify", None, conf)))
                    case Nil => retry(rejected)
                    case _ =>
                      val passing = cands.iterator
                        .map { js =>
                          attempts += 1; js
                        }
                        .find(verifies)
                      passing match {
                        case Some(js) =>
                          result("pass", Some(js), Some(conf), attempts)
                        case None => // reified but none covering target
                          val js = cands.head
                          retry(Some(Rejected("fail-verify", Some(js), conf)))
                      }
                  }
                case None =>
                  // symbolic execution returned no model: distinguish a genuine
                  // "unsolved" from one aborted by the per-side time limit
                  if (interp.timeout) timeoutResult(f, cond, attempts)
                  else rejectedResult(rejected, attempts)
              }
            }
            retry(None)
          } catch {
            case _: TimeoutException => timeoutResult(f, cond, attempts)
          }
        }

        val completion = ExecutorCompletionService[BranchResult](pool)
        val targetIter = targets.iterator
        val resultBuilder = List.newBuilder[BranchResult]
        var submitted = 0
        var completed = 0

        def submitNext(): Unit =
          if (targetIter.hasNext) {
            val (f, cond) = targetIter.next()
            completion.submit(new Callable[BranchResult] {
              def call(): BranchResult = solveTarget(f, cond)
            })
            submitted += 1
          }

        for (_ <- 0 until math.min(nThreads, targets.size)) submitNext()

        while (completed < targets.size) {
          val done = completion.poll(30, TimeUnit.SECONDS)
          if (done == null) {
            println(
              s"  progress: $completed / ${targets.size} completed " +
              s"($submitted submitted)",
            )
            // diagnostic: dump where the in-flight worker threads are stuck
            import scala.jdk.CollectionConverters.*
            for {
              (t, stack) <- Thread.getAllStackTraces.asScala.toList
              if t.getName.startsWith("builtin-branch-test")
            } {
              println(s"  [stuck] ${t.getName}")
              for (frame <- stack.take(20))
                println(s"      at $frame")
            }
          } else {
            val r = done.get()
            resultBuilder += r
            writeCaseLog(r)
            completed += 1
            submitNext()
          }
        }
        resultBuilder.result().sortBy(r => (r.bid, if (r.side) 0 else 1))
      }

      val verifiedResults = results.filter(_.status == "pass")
      val missedResults = results.filter(_.status == "fail-verify")
      val solved = verifiedResults.size + missedResults.size
      val verified = verifiedResults.size
      val timings =
        (verifiedResults ++ missedResults)
          .map(r => (r.fname, r.bid, r.side, r.elapsed, r.attempts))

      val statusOrder =
        List("pass", "fail-verify", "fail-reify", "unsolved", "timeout")
      val byStatus = results.groupBy(_.status)
      val orderedStatuses =
        statusOrder.filter(byStatus.contains) ++
        byStatus.keys.filterNot(statusOrder.contains).toList.sorted

      val solvedDir = s"$SOLVER_LOG_DIR/solved-programs"
      mkdir(solvedDir, remove = true)
      verifiedResults
        .sortBy(r => (r.bid, if (r.side) 0 else 1))
        .foreach { r =>
          r.js match
            case Some(js) =>
              val filename = s"${r.bid}-${sideString(r.side)}.js"
              dumpFile(js, s"$solvedDir/$filename")
            case None => ()
        }

      // emit the run summary to an arbitrary sink (console and/or dump file)
      def writeReport(out: String => Unit): Unit = {
        // per-status breakdown: count, share, and elapsed time per status tag
        val totalCount = results.size
        out("\n  Status breakdown:")
        for (status <- orderedStatuses) {
          val rs = byStatus(status)
          val totalS = rs.map(_.elapsed).sum / 1e9
          val avgS = totalS / rs.size
          val avgAttempts = rs.map(_.attempts).sum.toDouble / rs.size
          val pct = if (totalCount == 0) 0.0 else rs.size * 100.0 / totalCount
          out(
            f"    $status%-12s ${rs.size}%5d (${pct}%5.1f%%)  " +
            f"(total $totalS%8.1fs, avg $avgS%6.3fs, " +
            f"avg attempts $avgAttempts%5.1f)",
          )
        }
        val grandTotalS = results.map(_.elapsed).sum / 1e9
        val avgAttempts =
          if (totalCount == 0) 0.0
          else results.map(_.attempts).sum.toDouble / totalCount
        out(
          f"    ${"total"}%-12s $totalCount%5d (100.0%%)  " +
          f"(total $grandTotalS%8.1fs, avg attempts $avgAttempts%5.1f)",
        )
        // timing summary
        if (timings.nonEmpty) {
          out("\n  Top 10 slowest:")
          for {
            (name, bid, side, ns, attempts) <- timings.sortBy(-_._4).take(10)
          } out(
            f"    ${ns / 1e9}%8.3fs  " +
            f"${attempts}%4d attempts  " +
            s"$name Branch[$bid]:${sideString(side)}",
          )
          val totalS = timings.map(_._4).sum / 1e9
          val avgS = totalS / timings.size
          out(f"\n  Solve time: $totalS%.1fs total, $avgS%.3fs avg")
        }
        out("\n  Modeling AO targets:")
        out(f"    total unique:  $modelingAOCount%4d")
        out(f"    entry:         ${builtinEntryAONames.size}%4d")
        out(f"    non-entry:     ${nonEntryAONames.size}%4d")
      }

      writeReport(s => println(s))

      // dump summary
      val summaryFile = getPrintWriter(s"$SOLVER_LOG_DIR/summary")
      def section(title: String): Unit = {
        summaryFile.println()
        summaryFile.println("=" * 72)
        summaryFile.println(s"  $title")
        summaryFile.println("=" * 72)
      }
      try {
        section("SUMMARY")
        writeReport(s => summaryFile.println(s))

        section("BRANCH LIST BY STATUS")
        for (status <- orderedStatuses) {
          val rs = byStatus(status).sortBy(r => (r.bid, if (r.side) 0 else 1))
          summaryFile.println(s"\n  [$status] ${rs.size}")
          rs.foreach { r =>
            summaryFile.println(
              f"    Branch[${r.bid}]:${sideString(r.side)}  ${caseLabel(r)}",
            )
          }
        }

        section("SOLVE TIME (slowest first)")
        summaryFile.println(s"\n  [all] ${results.size}")
        results.sortBy(-_.elapsed).foreach { r =>
          summaryFile.println(
            f"    ${r.elapsed / 1e9}%8.3fs  ${r.status}%-12s " +
            f"${r.attempts}%4d attempts  " +
            f"Branch[${r.bid}]:${sideString(r.side)}  ${caseLabel(r)}",
          )
        }

        section("MODELING AO NAMES")
        summaryFile.println(s"\n  [entry] ${builtinEntryAONames.size}")
        builtinEntryAONames.foreach(name => summaryFile.println(s"    $name"))
        summaryFile.println(s"\n  [non-entry] ${nonEntryAONames.size}")
        nonEntryAONames.foreach(name => summaryFile.println(s"    $name"))
      } finally summaryFile.close()

      // -------------------------------------------------------------------------
      // XXX: remove later
      // -------------------------------------------------------------------------
      val dumpFail = getPrintWriter(s"$SOLVER_LOG_DIR/fail-verify-todo")
      try
        missedResults
          .filter(r => expectedInjection.contains((r.bid, r.side)))
          .sortBy(r => (r.conds.map(_.size).getOrElse(0), r.bid))
          .foreach { r =>
            dumpCase(s => dumpFail.println(s), r)
            dumpFail.println()
          }
      finally dumpFail.close()
      // -------------------------------------------------------------------------

      println(s"dumped to $SOLVER_LOG_DIR/")

      assert(solved > 0)
      assert(verified > 0)
    }

    check("builtin branches: summary") {
      val total = targets.size
      assert(total > 0)
      pool.shutdownNow()
    }
  }

  init
}
