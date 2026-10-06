package esmeta.solver

import esmeta.SOLVER_LOG_DIR
import esmeta.cfg.*
import esmeta.es.builtin.{INNER_CODE, intrAddr}
import esmeta.es.util.Coverage
import esmeta.es.util.Coverage.*
import esmeta.ir.{Func => _, *}
import esmeta.state.*
import esmeta.spec.*
import esmeta.solver.Solver.Invocation
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP, ProgressBar}
import esmeta.util.SystemUtils.*
import io.circe.Json
import java.util.concurrent.{
  ConcurrentHashMap => CMMap,
  ConcurrentLinkedQueue,
  TimeoutException,
}
import scala.collection.mutable.{
  ListBuffer,
  Map => MMap,
  Set => MSet,
  Stack,
  Queue,
}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** Solve selected branch sides */
class Solver(
  cfg: CFG,
  branch: Option[Int] = None,
  side: Option[Boolean] = None,
  log: Boolean = false,
  detail: Boolean = false,
) {
  private given CFG = cfg
  private val solveTimeLimit = 10
  private val solveTimeout = Duration(solveTimeLimit, "seconds")
  private lazy val analyzer: SymAnalyzer = {
    val an = new SymAnalyzer(cfg)
    an.analyze
    an
  }
  private lazy val templateGen = new TemplateGenerator(analyzer)
  private lazy val synth = ExprSynthesizer(cfg, templateGen.templatesBySlot)
  private lazy val cov = Coverage(cfg, timeLimit = Some(2))

  // branch sides each target's programs touched, shortest program per side
  private val touchedBy = CMMap[(Int, Boolean), Map[(Int, Boolean), String]]()

  // check for `yet`, ignoring assertions
  private def hasYet(elem: IRElem): Boolean = elem match
    case IAssert(_) => false
    case _ =>
      var found = false
      val walker = new esmeta.ir.util.UnitWalker {
        override def walk(expr: Expr): Unit = expr match
          case _: EYet => found = true
          case _       => super.walk(expr)
      }
      walker.walk(elem)
      found

  // resolve explicitly named calls
  private def directCallee(call: Call): Option[Func] = call.callInst match
    case ICall(_, EClo(name, _), _) => cfg.fnameMap.get(name)
    case _                          => None

  // propagate `yet` barriers with optimistic return summaries
  private lazy val reachableNodesByFunc: Map[Int, Set[Int]] = {
    var mayReturn = cfg.funcs.toSet
    var reachable = Map.empty[Int, Set[Int]]
    var changed = true
    while (changed) {
      val nextMayReturn = MSet.empty[Func]
      reachable = cfg.funcs.map { f =>
        val seen = MSet(f.entry.id)
        val stack = Stack[Node](f.entry)
        while (stack.nonEmpty) {
          val n = stack.pop()
          val blocked = n match
            case b: Branch                     => hasYet(b.cond)
            case b: Block                      => b.insts.exists(hasYet)
            case c: Call if hasYet(c.callInst) => true
            case c: Call =>
              directCallee(c) match
                case Some(callee) => !mayReturn.contains(callee)
                case None         => false // Indirect calls may return.
          // include blocking nodes without traversing their successors
          if (!blocked) {
            if (f.exits(n)) nextMayReturn += f
            for (m <- n.succs if seen.add(m.id)) stack.push(m)
          }
        }
        f.id -> seen.toSet
      }.toMap
      val next = nextMayReturn.toSet
      changed = next != mayReturn
      mayReturn = next
    }
    reachable
  }

  // direct calls reachable before a `yet` barrier
  private def reachableCallees(f: Func): Set[Func] =
    val reachable = reachableNodesByFunc(f.id)
    f.nodes
      .collect { case c: Call if reachable(c.id) && !hasYet(c.callInst) => c }
      .flatMap(directCallee)
      .toSet

  // call distances for target selection and entry ordering
  private def callDistances(entry: Func): Map[Func, Int] = {
    val distance = MMap(entry -> 0)
    val queue = Queue(entry)
    while (queue.nonEmpty)
      val caller = queue.dequeue()
      for (callee <- reachableCallees(caller) if !distance.contains(callee)) {
        distance(callee) = distance(caller) + 1
        queue.enqueue(callee)
      }
    distance.toMap
  }

  // exclude boilerplate, constant, and unsupported branches
  private lazy val candidateBranches: Set[Int] =
    cfg.nodes.flatMap {
      case b: Branch if !b.isFiltered && !hasYet(b.cond) =>
        b.cond match
          case EBool(_) => None // skip constant branches
          case _        => Some(b.id)
      case _ => None
    }.toSet

  /** select static builtin targets before analysis */
  lazy val targets: List[(List[Func], Cond)] = {
    if (side.nonEmpty && branch.isEmpty)
      raise("solve: -solve:side requires -solve:branch")
    for (id <- branch)
      cfg.nodeMap.get(id) match
        case Some(_: Branch) => ()
        case _               => raise(s"solve: node $id is not a branch")
    val builtins = cfg.funcs
      .filter { f =>
        f.isBuiltin && Solver.funcAccessExpr(f).nonEmpty &&
        cfg.init.initHeap.map
          .get(intrAddr(f.name.stripPrefix("INTRINSICS.")))
          .exists {
            case obj: RecordObj =>
              (obj.map.contains("Call") || obj.map.contains("Construct")) &&
              obj.map.get(INNER_CODE).exists {
                case Clo(code, _) => code == f
                case _            => false
              }
            case _ => false
          }
      }
      .sortBy(_.name)
    val pairs = for {
      entry <- builtins
      (func, distance) <- callDistances(entry).toList
      target <- func.nodes.collect {
        case b: Branch
            if reachableNodesByFunc(func.id)(b.id) && candidateBranches(b.id) =>
          b
      }
    } yield target -> (entry, distance)
    val selected = pairs
      .groupMap(_._1)(_._2)
      .toList
      .sortBy(_._1.id)
      .filter((b, _) => branch.forall(_ == b.id))
      .flatMap { (b, candidates) =>
        val entries = candidates.sortBy((f, d) => (d, f.id)).map(_._1)
        List(true, false)
          .filter(s => side.forall(_ == s))
          .map(s => entries -> Cond(b, s))
      }
    if (selected.isEmpty)
      raise(branch.fold("solve: no targets") { id =>
        s"solve: branch $id is outside the target set"
      })
    selected
  }

  lazy val logDir: Option[String] = Option.when(log) {
    val dir = s"$SOLVER_LOG_DIR/solve-$dateStr"
    mkdir(dir, remove = true)
    createSymLink(s"$SOLVER_LOG_DIR/recent", dir, overwrite = true)
    dumpFile(getSeed, s"$dir/seed")
    dir
  }

  // outcomes and their summary, kept from solve for report
  private val completed = ConcurrentLinkedQueue[BranchResult]()
  private val stats = ConcurrentLinkedQueue[TargetStat]()

  // clock for statistics, read only in logging mode
  private inline def now: Long = if (log) System.nanoTime() else 0L
  private var summary = ""

  /** solve the targets, and return a witness of each branch side touched */
  lazy val solve: Map[(Int, Boolean), String] = {
    val selected = targets
    val targetKeys = selected.map { (_, c) => (c.branch.id, c.cond) }.toSet
    logDir
    synth
    for (dir <- logDir)
      dumpJson(
        name = "candidate synthesis templates",
        data = templateGen.json,
        filename = s"$dir/templates.json",
      )
    val nThreads = Runtime.getRuntime.availableProcessors
    val solving = ProgressBar(
      msg = s"solving with $nThreads threads (timeout: $solveTimeout/target)",
      iterable = selected,
      verbose = branch.isEmpty,
      detail = false,
      concurrent = CP.Fixed(nThreads),
    )
    solving.foreach { (entries, cond) =>
      val seed = (getSeed, cond.branch.id, cond.cond).hashCode
      val r = withSeed(seed)(solveTarget(entries, cond))
      completed.add(r)
    }
    val merged = MMap[(Int, Boolean), String]()
    for {
      found <- touchedBy.values.asScala
      (c, js) <- found
    } merged(c) = merged.get(c).fold(js)(shorter(_, js))
    val witnesses = merged.toMap
    for (dir <- logDir)
      dumpFile(
        name = "solver statistics",
        data = stats.asScala.toList
          .sortBy(s => (s.cond.branch.id, !s.cond.cond))
          .map(_.json(witnesses).noSpaces)
          .mkString("", "\n", "\n"),
        filename = s"$dir/stats.jsonl",
      )
    val conds = witnesses.keySet
    val reached = (conds intersect targetKeys).size
    val outside = (conds -- targetKeys).size
    val reachedPct = reached * 100.0 / selected.size
    summary = templateGen.summary +
      s"Solving: ${solving.summary.time.simpleString}\n" +
      "Status breakdown:\n" +
      statusGroups(outcomes(witnesses)).map { (status, rs) =>
        val count = rs.size
        val pct = count * 100.0 / selected.size
        f"  $status%-12s $count%5d (${pct}%5.1f%%)\n"
      }.mkString +
      f"\nReached targets: $reached/${selected.size} (${reachedPct}%.1f%%)\n" +
      s"Observed outside target set: $outside\n"
    if (branch.isEmpty) print(summary)
    witnesses
  }

  /** dump the summary, and list the outcomes when one branch is targeted */
  def report(witnesses: Map[(Int, Boolean), String]): Unit = {
    val results = outcomes(witnesses)
    for (dir <- logDir) {
      val details = statusGroups(results).map { (status, rs) =>
        rs.map(r => s"  ${r.cond}  ${cfg.funcOf(r.cond.branch).name}")
          .mkString(s"\n[$status] ${rs.size}\n", "\n", "\n")
      }.mkString
      dumpFile(
        name = "solver summary",
        data = summary + details,
        filename = s"$dir/summary",
      )
    }
    if (branch.nonEmpty)
      for (r <- results)
        println(s"[${r.status}] ${r.cond}: ${r.js.getOrElse("no program")}")
  }

  // count all observed targets as passes
  private def outcomes(
    witnesses: Map[(Int, Boolean), String],
  ): List[BranchResult] = completed.asScala.toList
    .map { r =>
      witnesses.get((r.cond.branch.id, r.cond.cond)) match
        case Some(js) => r.copy(status = "pass", js = Some(js))
        case None     => r
    }
    .sortBy { r => (r.cond.branch.id, if (r.cond.cond) 0 else 1) }

  private def statusGroups(
    results: List[BranchResult],
  ): List[(String, List[BranchResult])] =
    val byStatus = results.groupBy(_.status)
    List("pass", "fail-verify", "fail-reify", "unsolved", "timeout", "error")
      .flatMap(status => byStatus.get(status).map(status -> _))

  // ---------------------------------------------------------------------------
  // solving and concrete verification
  // ---------------------------------------------------------------------------

  private def solveTarget(entries: List[Func], cond: Cond): BranchResult = {
    val start = System.nanoTime()
    val targetDeadline = start + solveTimeout.toNanos
    val perEntry = solveTimeout.toNanos / entries.size
    val stat = TargetStat(cond, entries.size)
    val key = (cond.branch.id, cond.cond)
    val found = MMap[(Int, Boolean), String]()
    def solveNext(f: Func): BranchResult = {
      val deadline = (System.nanoTime() + perEntry).min(targetDeadline)
      val entryStat = EntryStat(f.name)
      if (log) stat.entries += entryStat
      val entryStart = now
      val r =
        try solveEntry(f, cond, deadline, entryStat, found)
        catch {
          case e: Throwable =>
            println(s"[error] ${f.name} -> $cond  $e")
            BranchResult(cond, "error")
        }
      entryStat.ns = now - entryStart
      entryStat.status = r.status
      r
    }
    def rank(status: String): Int = status match
      case "pass"        => 0
      case "fail-verify" => 1
      case "fail-reify"  => 2
      case "timeout"     => 3
      case "unsolved"    => 4
      case _             => 5
    val rest = entries.iterator
    var best = solveNext(rest.next())
    while (
      best.status != "pass" &&
      rest.hasNext &&
      System.nanoTime() < targetDeadline
    ) {
      val other = solveNext(rest.next())
      if (rank(other.status) < rank(best.status)) best = other
    }
    stat.status = best.status
    stat.js = best.js
    stat.ns = now - start
    if (log) stats.add(stat)
    touchedBy.put(key, found.toMap)
    best
  }

  private def solveEntry(
    f: Func,
    cond: Cond,
    deadline: Long,
    entryStat: EntryStat,
    found: MMap[(Int, Boolean), String],
  ): BranchResult = {
    def expired: Boolean = System.nanoTime() > deadline
    given checkTimeout: (() => Unit) =
      () => if (expired) throw TimeoutException("solver")
    val interp = new SymInterpreter(
      analyzer,
      f,
      cond.branch,
      side = Some(cond.cond),
      timeLimit = Some(solveTimeLimit),
      detail = detail,
      checkDeadline = checkTimeout,
    )
    // sample each distinct invocation once per entry
    val invocations = Iterator
      .unfold(())(_ => interp.nextCandidate.map(_ -> ()))
      .map { conf =>
        entryStat.symPaths += 1
        Solver.getInvocation(interp.analyzer)(f, conf.state)
      }
      .distinct
    // the path being tried, closed when it ends or the deadline expires
    var current: Option[PathStat] = None
    def close(p: PathStat, outcome: String): Unit =
      p.outcome = outcome
      p.ns = now - p.start
      current = None
    @scala.annotation.tailrec
    def retry(rejected: Option[BranchResult]): BranchResult = {
      checkTimeout()
      val before = entryStat.symPaths
      val searchStart = now
      val next = invocations.nextOption
      entryStat.searchNs += now - searchStart
      next match {
        case Some(invocation) =>
          val p = PathStat(entryStat.paths.size)
          p.symPaths = entryStat.symPaths - before
          p.reified = invocation.flatMap(_.form).isDefined
          if (log) entryStat.paths += p
          current = Some(p)
          val seen = MSet.empty[String]
          val candidates = invocation
            .to(LazyList)
            .flatMap { invocation =>
              LazyList
                .from(0)
                .map { attempt =>
                  checkTimeout()
                  assemble(invocation, first = attempt == 0)
                }
                .takeWhile(_.isDefined)
                .flatten
                .flatMap(_.to(LazyList))
            }
            .map(_ + ";")
            .take(Solver.maxCandidatesPerPath)
            .map { js =>
              p.generated += 1
              js
            }
            .filter(seen.add)
          candidates.headOption match {
            case Some(_) =>
              val passing = candidates.iterator.find { js =>
                val execStart = now
                p.aborted = true
                val result = touched(js, checkTimeout)
                p.aborted = false
                p.executed += 1
                p.execNs += now - execStart
                val conds = result.getOrElse { p.errors += 1; Set.empty }
                for (c <- conds)
                  if (!found.contains(c) && c != (cond.branch.id, cond.cond))
                    p.incidental += 1
                  found(c) = found.get(c).fold(js)(shorter(_, js))
                conds((cond.branch.id, cond.cond))
              }
              passing match {
                case Some(js) =>
                  close(p, "pass")
                  BranchResult(cond, "pass", Some(js))
                case None =>
                  close(p, "fail")
                  val next = rejected
                    .filter(_.status == "fail-verify")
                    .orElse(Some(BranchResult(cond, "fail-verify")))
                  retry(next)
              }
            case None =>
              close(p, "no-candidate")
              retry(rejected.orElse(Some(BranchResult(cond, "fail-reify"))))
          }
        case None =>
          entryStat.exhausted = !expired
          if (expired) BranchResult(cond, "timeout")
          else rejected.getOrElse(BranchResult(cond, "unsolved"))
      }
    }
    try retry(None)
    catch {
      case _: TimeoutException =>
        for (p <- current) close(p, "timeout")
        BranchResult(cond, "timeout")
    }
  }

  /** the shorter program, the smaller one on a tie */
  private def shorter(a: String, b: String): String =
    if (a.length < b.length || (a.length == b.length && a <= b)) a else b

  /** assemble a target call from synthesized input expressions */
  private def assemble(invocation: Invocation, first: Boolean)(using
    checkDeadline: () => Unit,
  ): Option[List[String]] = {
    checkDeadline()
    invocation.form.flatMap { (_, holes) =>
      synth.synthesize(holes, first).map(invocation.candidates)
    }
  }

  /** branch sides touched by a program */
  private def touched(
    js: String,
    checkTimeout: () => Unit,
  ): Option[Set[(Int, Boolean)]] = {
    checkTimeout()
    try {
      val interp = Coverage.Interp(
        cfg.init.from(js),
        cov.tyCheck,
        cov.kFs,
        cov.cp,
        cov.timeLimit,
        cov.isTargetNode,
        cov.isTargetBranch,
      )
      interp.result
      checkTimeout()
      Some((for {
        cv <- interp.touchedCondViews.keys
      } yield (cv.cond.branch.id, cv.cond.cond)).toSet)
    } catch {
      case e: TimeoutException => throw e
      case _: Throwable        => None
    }
  }

  private case class BranchResult(
    cond: Cond,
    status: String,
    js: Option[String] = None,
  )

  // ---------------------------------------------------------------------------
  // statistics (stats.jsonl in the log directory, one target side per line)
  // ---------------------------------------------------------------------------

  private def ms(ns: Long): Json = Json.fromDoubleOrNull(ns / 1e6)

  /** one distinct invocation (path) tried for an entry */
  private class PathStat(val index: Int) {
    val start = now
    var symPaths = 0 // symbolic paths found, including duplicate invocations
    var reified = false // has an invocation form
    var generated = 0 // programs taken within the per-path budget
    var executed = 0 // programs executed after deduplication
    var aborted = false // an execution cut off by the deadline
    var errors = 0 // executions that threw or hit the coverage time limit
    var incidental = 0 // other branch sides first covered within the target
    var outcome = "" // pass | fail | no-candidate | timeout
    var execNs = 0L
    var ns = 0L
    def json: Json = Json.obj(
      "index" -> Json.fromInt(index),
      "symPaths" -> Json.fromInt(symPaths),
      "reified" -> Json.fromBoolean(reified),
      "generated" -> Json.fromInt(generated),
      "executed" -> Json.fromInt(executed),
      "aborted" -> Json.fromBoolean(aborted),
      "errors" -> Json.fromInt(errors),
      "incidental" -> Json.fromInt(incidental),
      "outcome" -> Json.fromString(outcome),
      "execMs" -> ms(execNs),
      "ms" -> ms(ns),
    )
  }

  /** one entry function tried for a target side */
  private class EntryStat(val name: String) {
    val paths = ListBuffer[PathStat]()
    var symPaths = 0
    var exhausted = false // no more paths before the deadline
    var status = ""
    var searchNs = 0L
    var ns = 0L
    def json: Json = Json.obj(
      "name" -> Json.fromString(name),
      "status" -> Json.fromString(status),
      "symPaths" -> Json.fromInt(symPaths),
      "exhausted" -> Json.fromBoolean(exhausted),
      "searchMs" -> ms(searchNs),
      "ms" -> ms(ns),
      "paths" -> Json.fromValues(paths.map(_.json)),
    )
  }

  /** one target branch side */
  private class TargetStat(
    val cond: Cond,
    val nEntries: Int,
  ) {
    val entries = ListBuffer[EntryStat]()
    var status = ""
    var js: Option[String] = None // the program the target itself found
    var ns = 0L
    def json(witnesses: Map[(Int, Boolean), String]): Json = Json.obj(
      "branch" -> Json.fromInt(cond.branch.id),
      "side" -> Json.fromBoolean(cond.cond),
      "func" -> Json.fromString(cfg.funcOf(cond.branch).name),
      "nEntries" -> Json.fromInt(nEntries),
      "status" -> Json.fromString(status),
      "program" -> js.fold(Json.Null)(Json.fromString),
      "covered" -> Json.fromBoolean(
        witnesses.contains((cond.branch.id, cond.cond)),
      ),
      "ms" -> ms(ns),
      "entries" -> Json.fromValues(entries.map(_.json)),
    )
  }
}

object Solver {

  val maxCandidatesPerPath: Int = 100

  /** extract a template and its input types */
  def getInvocation(analyzer: SymAnalyzer)(
    entryFunc: Func,
    st: analyzer.AbsState,
  ): Option[Invocation] =
    import analyzer.*
    // get constraints for each symbolic input
    val thisTy = st.getConstr(SThis.sym)
    val newTargetTy = st.getConstr(SNewTarget.sym)
    // newTarget alone does not imply a constructable entry
    val newTarget =
      if (isConstructable(entryFunc, analyzer.cfg)) newTargetTy
      else newTargetTy && UndefT
    if (newTarget.isBottom) return None
    entryFunc.head match
      case Some(head: BuiltinHead) =>
        val paramTys = head.params.zipWithIndex.map { (param, i) =>
          if (param.kind == ParamKind.Variadic) st.getConstr(SArgs.sym)
          else st.getConstr(i)
        }
        val variadicTys =
          if (!head.params.exists(_.kind == ParamKind.Variadic)) Nil
          else {
            val listTy = st.getConstr(SArgs.sym).list
            if (listTy.isBottom) return None
            val elemTy = listTy.elem && ESValueT
            st.symEnv.keysIterator
              .flatMap(variadicIdxOf)
              .maxOption
              .toList
              .flatMap { last =>
                (0 to last).map { i =>
                  st.getConstr(SVariadicIdx(i).sym) && elemTy
                }
              }
          }
        Some(Invocation(head, thisTy, paramTys, variadicTys, newTarget))
      case _ => None

  object Invocation {
    private val holePattern =
      "#(?:THIS|NEW_TARGET|VAR\\[[0-9]+\\]|[0-9]+)".r

    /** replace holes with already synthesized expressions */
    def fill(expr: String, values: Map[String, String]): String =
      holePattern.replaceAllIn(
        expr,
        m => scala.util.matching.Regex.quoteReplacement(values(m.matched)),
      )
  }

  /** builtin signature and symbolic input types */
  case class Invocation(
    head: BuiltinHead,
    thisTy: ValueTy,
    paramTys: List[ValueTy],
    variadicTys: List[ValueTy],
    newTargetTy: ValueTy,
  ) {

    private val path = head.path

    private val inputs = head.params.zip(paramTys).zipWithIndex.flatMap {
      case ((param, ty), i) =>
        if (param.kind == ParamKind.Variadic)
          variadicTys.zipWithIndex.map((ty, k) => s"#VAR[$k]" -> ty)
        else List(s"#$i" -> ty)
    }

    val form: Option[(String, List[(String, ValueTy)])] = formFor(inputs)

    def candidates(values: Map[String, String]): List[String] = {
      val count =
        inputs.reverse.takeWhile((hole, _) => values(hole) == "undefined").size
      (count to 0 by -1).toList.flatMap { n =>
        formFor(inputs.dropRight(n)).map((expr, _) =>
          Invocation.fill(expr, values),
        )
      }.distinct
    }

    private def formFor(
      argHoles: List[(String, ValueTy)],
    ): Option[(String, List[(String, ValueTy)])] = {
      val fixed = head.params.takeWhile(_.kind != ParamKind.Variadic).size
      val args = argHoles.take(fixed).map(_._1) ++
        Option
          .when(fixed < head.params.size) {
            argHoles.drop(fixed).map(_._1).mkString("...[", ", ", "]")
          }
          .toList
      if (UndefT ⊑ newTargetTy)
        call("#THIS", args).map(_ -> (("#THIS" -> thisTy) :: argHoles))
      else {
        val ctorTy = newTargetTy && ConstructorT
        if (ctorTy.isBottom) None
        else
          construct(args, "#NEW_TARGET")
            .map(_ -> (argHoles :+ ("#NEW_TARGET" -> ctorTy)))
      }
    }

    private def call(receiver: String, args: List[String]): Option[String] =
      path match
        case BuiltinPath.Getter(base) =>
          descriptor(base).map(d => s"$d.get.call($receiver)")
        case BuiltinPath.Setter(base) =>
          val values = (receiver :: args).mkString(", ")
          descriptor(base).map(d => s"$d.set.call($values)")
        case _ =>
          val values = (receiver :: args).mkString(", ")
          access(path).map(fn => s"$fn.call($values)")

    private def construct(
      args: List[String],
      newTarget: String,
    ): Option[String] =
      access(path).map { fn =>
        s"Reflect.construct($fn, [${args.mkString(", ")}], $newTarget)"
      }
  }

  def isConstructable(func: Func, cfg: CFG): Boolean =
    cfg.init.intrHeap
      .get(intrAddr(func.name.stripPrefix("INTRINSICS.")))
      .exists {
        case record: RecordObj => record.map.contains("Construct")
        case _                 => false
      }

  def getPath(func: Func): Option[BuiltinPath] = func.head match {
    case Some(h: BuiltinHead) => Some(h.path)
    case _                    => None
  }

  // JS expression accessing an exposed builtin function value
  def funcAccessExpr(f: Func): Option[String] =
    if (!f.isBuiltin) None
    else
      getPath(f)
        .orElse {
          Option.when(f.name.startsWith("INTRINSICS.")) {
            BuiltinPath.from(f.name.stripPrefix("INTRINSICS."))
          }
        }
        .flatMap {
          case BuiltinPath.Getter(base) => descriptor(base).map(_ + ".get")
          case BuiltinPath.Setter(base) => descriptor(base).map(_ + ".set")
          case path                     => access(path)
        }

  // JS expression accessing the builtin at path
  private def access(path: BuiltinPath): Option[String] = path match
    case BuiltinPath.Base(name) =>
      globalAlias.get(name) match
        case Some("")   => None // intrinsic not exposed to JS code
        case Some(expr) => Some(expr)
        case None       => Some(name) // directly nameable global
    case BuiltinPath.NormalAccess(base, name) =>
      access(base).map(b => s"$b.$name")
    case BuiltinPath.SymbolAccess(base, sym) =>
      access(base).map(b => s"$b[Symbol.$sym]")
    case BuiltinPath.Getter(base) => access(base)
    case BuiltinPath.Setter(base) => access(base)

  // Object.getOwnPropertyDescriptor(target, key) for a getter/setter base
  private def descriptor(base: BuiltinPath): Option[String] = base match
    case BuiltinPath.NormalAccess(b, n) =>
      val target = access(b)
      val key = s"\"${normStr(n)}\""
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case BuiltinPath.SymbolAccess(b, s) =>
      val target = access(b)
      val key = s"Symbol.$s"
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case _ => None

  // intrinsic access paths (test262/harness/wellKnownIntrinsicObjects.js)
  private val globalAlias: Map[String, String] = Map(
    "TypedArray" -> "Object.getPrototypeOf(Uint8Array)",
    "ArrayIteratorPrototype" -> "Object.getPrototypeOf([][Symbol.iterator]())",
    "AsyncFromSyncIteratorPrototype" -> "",
    "AsyncFunction" -> "(async function() {}).constructor",
    "AsyncGeneratorFunction" -> "(async function* () {}).constructor",
    "AsyncGeneratorPrototype" -> "Object.getPrototypeOf(async function* () {}).prototype",
    "AsyncIteratorPrototype" -> "Object.getPrototypeOf(Object.getPrototypeOf(async function* () {}).prototype)",
    "ForInIteratorPrototype" -> "",
    "GeneratorFunction" -> "(function* () {}).constructor",
    "GeneratorPrototype" -> "Object.getPrototypeOf(function * () {}).prototype",
    "IteratorHelperPrototype" -> "Object.getPrototypeOf(Iterator.from([]).drop(0))",
    "MapIteratorPrototype" -> "Object.getPrototypeOf(new Map()[Symbol.iterator]())",
    "SetIteratorPrototype" -> "Object.getPrototypeOf(new Set()[Symbol.iterator]())",
    "StringIteratorPrototype" -> "Object.getPrototypeOf(new String()[Symbol.iterator]())",
    "RegExpStringIteratorPrototype" -> """Object.getPrototypeOf(RegExp.prototype[Symbol.matchAll](""))""",
    "WrapForValidIteratorPrototype" -> "Object.getPrototypeOf(Iterator.from({ [Symbol.iterator](){ return {}; } }))",
    "ThrowTypeError" -> """(function() { "use strict"; return Object.getOwnPropertyDescriptor(arguments, "callee").get })()""",
  )
}
