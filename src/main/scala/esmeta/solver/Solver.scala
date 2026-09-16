package esmeta.solver

import esmeta.SOLVER_LOG_DIR
import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.*
import esmeta.es.builtin.{INNER_CODE, intrAddr}
import esmeta.es.util.Coverage
import esmeta.es.util.Coverage.*
import esmeta.ir.{Func => _, *}
import esmeta.state.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP, ProgressBar}
import esmeta.util.SystemUtils.*
import java.util.concurrent.{
  ConcurrentHashMap => CMMap,
  ConcurrentLinkedQueue,
  TimeoutException,
}
import scala.collection.mutable.{Map => MMap, Set => MSet, Stack, Queue}
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
  private lazy val tyChecker: TyChecker = {
    val analyzer = TyChecker(cfg, silent = true)
    analyzer.analyze
    analyzer
  }
  private lazy val synthesizer: TySynthesizer = TySynthesizer(cfg, tyChecker)
  private lazy val cov = Coverage(cfg, timeLimit = Some(2))

  // branch-side witnesses with a builtin as the nearest feature
  private val condMap = CMMap[(Int, Boolean), String]()

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
    cfg.nodes.collect {
      case b: Branch
          if !b.isFiltered && !hasYet(b.cond) && !b.cond.isInstanceOf[EBool] =>
        b.id
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
      raise(branch.fold("solve: no static builtin branch targets") { id =>
        s"solve: branch $id is outside the static builtin target set"
      })
    selected
  }

  private lazy val logDir: Option[String] = Option.when(log) {
    val dir = s"$SOLVER_LOG_DIR/solve-$dateStr"
    mkdir(dir, remove = true)
    createSymLink(s"$SOLVER_LOG_DIR/recent", dir, overwrite = true)
    dir
  }

  /** solve selected targets with a per-target budget */
  lazy val result: String = {
    val selected = targets
    val targetKeys = selected.map { (_, c) => (c.branch.id, c.cond) }.toSet
    logDir
    synthesizer
    val nThreads = Runtime.getRuntime.availableProcessors
    val completed = ConcurrentLinkedQueue[BranchResult]()
    ProgressBar(
      msg = s"solving with $nThreads threads ($solveTimeout per target)",
      iterable = selected,
      verbose = branch.isEmpty,
      detail = false,
      concurrent = CP.Fixed(nThreads),
    ).foreach { (entries, cond) =>
      // reuse known coverage without changing the target set
      val r = Option(condMap.get((cond.branch.id, cond.cond))) match
        case Some(js) => BranchResult(cond, "pass", Some(js))
        case None     => solveTarget(entries, cond)
      completed.add(r)
    }
    val conds = condMap.keySet.asScala.toSet
    // count all observed targets as passes
    val results = completed.asScala.toList
      .map { r =>
        Option(condMap.get((r.cond.branch.id, r.cond.cond))) match
          case Some(js) => r.copy(status = "pass", js = Some(js))
          case None     => r
      }
      .sortBy { r => (r.cond.branch.id, if (r.cond.cond) 0 else 1) }
    val reached = (conds intersect targetKeys).size
    val outside = (conds -- targetKeys).size
    val byStatus = results.groupBy(_.status)
    val statusGroups =
      List("pass", "fail-verify", "fail-reify", "unsolved", "timeout", "error")
        .flatMap(status => byStatus.get(status).map(status -> _))
    val reachedPct = reached * 100.0 / selected.size
    val summary = "Status breakdown:\n" +
      statusGroups.map { (status, rs) =>
        val count = rs.size
        val pct = count * 100.0 / selected.size
        f"  $status%-12s $count%5d (${pct}%5.1f%%)\n"
      }.mkString +
      f"\nReached targets: $reached/${selected.size} (${reachedPct}%.1f%%)\n" +
      s"Observed outside target set: $outside\n"

    // dump programs and coverage after solving
    for (dir <- logDir) {
      val witnesses = results.flatMap(r => r.js.map(r.cond -> _))
      val programs = witnesses
        .map(_._2)
        .distinct
        .zipWithIndex
        .map { (js, index) => js -> (index + 1) }
      val programIds = programs.toMap
      dumpDir[(String, Int)](
        name = s"${programs.size} ECMAScript programs",
        iterable = programs,
        dirname = s"$dir/programs",
        getName = { case (_, id) => s"$id.js" },
        getData = { case (js, _) => js },
      )
      // reuse the Fuzzer coverage format
      import cov.jsonProtocol.given
      val coverage = witnesses.zipWithIndex.map {
        case ((cond, js), index) =>
          CondViewInfo(
            index,
            CondView(cond, None),
            s"programs/${programIds(js)}.js",
          )
      }
      dumpJson(
        name = "branch coverage",
        data = coverage,
        filename = s"$dir/branch-coverage.json",
      )
      val details = statusGroups.map { (status, rs) =>
        rs.map(r => s"  ${r.cond}  ${cfg.funcOf(r.cond.branch).name}")
          .mkString(s"\n[$status] ${rs.size}\n", "\n", "\n")
      }.mkString
      dumpFile(
        name = "solver summary",
        data = summary + details,
        filename = s"$dir/summary",
      )
    }
    if (branch.isEmpty) summary
    else
      results
        .map(r => s"[${r.status}] ${r.cond}: ${r.js.getOrElse("no program")}")
        .mkString("", "\n", "\n")
  }

  // ---------------------------------------------------------------------------
  // solving and concrete verification
  // ---------------------------------------------------------------------------

  private def solveTarget(entries: List[Func], cond: Cond): BranchResult = {
    val targetDeadline = System.nanoTime() + solveTimeout.toNanos
    val perEntry = solveTimeout.toNanos / entries.size
    def solveNext(f: Func): BranchResult = {
      val deadline = (System.nanoTime() + perEntry).min(targetDeadline)
      try solveEntry(f, cond, deadline)
      catch {
        case e: Throwable =>
          println(s"[error] ${f.name} -> $cond  $e")
          BranchResult(cond, "error")
      }
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
    best
  }

  private def solveEntry(f: Func, cond: Cond, deadline: Long): BranchResult = {
    def expired: Boolean = System.nanoTime() > deadline
    given checkTimeout: (() => Unit) =
      () => if (expired) throw TimeoutException("solver")
    val interp = new SymInterp(
      tyChecker,
      f,
      cond.branch,
      side = Some(cond.cond),
      timeLimit = Some(solveTimeLimit),
      detail = detail,
      checkDeadline = checkTimeout,
    )
    @scala.annotation.tailrec
    def retry(rejected: Option[BranchResult]): BranchResult = {
      checkTimeout()
      interp.nextCandidate match {
        case Some(conf) =>
          val seen = MSet.empty[String]
          val candidates = Solver
            .getTemplate(interp.tychecker)(f, conf.state)
            .to(LazyList)
            .flatMap { template =>
              LazyList
                .continually {
                  checkTimeout()
                  template.instantiate(ty => synthesizer.synthesize(ty))
                }
                .takeWhile(_.isDefined)
                .flatten
            }
            .map(_ + ";")
            .take(Solver.maxCandidatesPerPath)
            .filter(seen.add)
          candidates.headOption match {
            case Some(_) =>
              val passing = candidates.iterator.find { js =>
                verifies(js, cond, checkTimeout)
              }
              passing match {
                case Some(js) => BranchResult(cond, "pass", Some(js))
                case None =>
                  val next = rejected
                    .filter(_.status == "fail-verify")
                    .orElse(Some(BranchResult(cond, "fail-verify")))
                  retry(next)
              }
            case None =>
              retry(rejected.orElse(Some(BranchResult(cond, "fail-reify"))))
          }
        case None =>
          if (expired) BranchResult(cond, "timeout")
          else rejected.getOrElse(BranchResult(cond, "unsolved"))
      }
    }
    try retry(None)
    catch { case _: TimeoutException => BranchResult(cond, "timeout") }
  }

  private def verifies(
    js: String,
    cond: Cond,
    checkTimeout: () => Unit,
  ): Boolean = {
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
      for (
        cv <- interp.touchedCondViews.keys
        if candidateBranches(cv.cond.branch.id)
      )
        condMap.putIfAbsent((cv.cond.branch.id, cv.cond.cond), js)
      interp.touchedCondViews.keys.exists { cv =>
        cv.cond.branch.id == cond.branch.id && cv.cond.cond == cond.cond
      }
    } catch {
      case e: TimeoutException => throw e
      case _: Throwable        => false
    }
  }

  private case class BranchResult(
    cond: Cond,
    status: String,
    js: Option[String] = None,
  )
}

object Solver {

  val maxCandidatesPerPath: Int = 100

  /** extract a template and its input types */
  def getTemplate(tychecker: TyChecker)(
    entryFunc: Func,
    st: tychecker.AbsState,
  ): Option[Template] =
    import tychecker.*
    given CFG = tychecker.cfg
    given AbsState = st
    // get constraints for each symbolic input
    val thisTy = st.getConstr(SThis.sym)
    val newTargetTy = st.getConstr(SNewTarget.sym)
    // newTarget alone does not imply a constructable entry
    val newTarget =
      if (isConstructable(entryFunc, cfg)) newTargetTy
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
        Some(Template(head, thisTy, paramTys, variadicTys, newTarget))
      case _ => None

  /** builtin signature and symbolic input types */
  case class Template(
    head: BuiltinHead,
    thisTy: ValueTy,
    paramTys: List[ValueTy],
    variadicTys: List[ValueTy],
    newTargetTy: ValueTy,
  )(using cfg: CFG) {

    private val path = head.path

    // resolve the callee's type without executing a generated program
    private lazy val calleeTy: ValueTy =
      cfg.init.initHeap.map.get(intrAddr(path.toString)) match
        case Some(obj: RecordObj) =>
          State(cfg, Context(cfg.main), heap = cfg.init.initHeap).typeOf(obj)
        case _ => BotT

    /** fill the invocation inputs with type-directed synthesis */
    def instantiate(
      synthesize: ValueTy => Option[String],
    )(using checkDeadline: () => Unit): Option[String] = {
      def arguments(): Option[List[String]] = {
        head.params
          .zip(paramTys)
          .foldLeft(Option(List.empty[String])) {
            case (values, (param, ty)) =>
              values.flatMap { vs =>
                checkDeadline()
                val expr =
                  if (param.kind != ParamKind.Variadic) synthesize(ty)
                  else
                    variadicTys
                      .foldLeft(Option(List.empty[String])) {
                        case (elements, elemTy) =>
                          elements.flatMap { es =>
                            checkDeadline()
                            synthesize(elemTy).map(_ :: es)
                          }
                      }
                      .map(_.reverse.mkString("...[", ", ", "]"))
                expr.map(_ :: vs)
              }
          }
          .map(_.reverse)
      }
      checkDeadline()
      val ctorTy = newTargetTy && ConstructorT
      val calls = Option
        .when(UndefT ⊑ newTargetTy) { () =>
          for {
            receiver <- synthesize(thisTy)
            args <- arguments()
            expr <- apply(receiver, args, "")
          } yield expr
        }
        .toList
      val constructs =
        if (ctorTy.isBottom) Nil
        else
          Option
            .when(!calleeTy.isBottom && calleeTy <= ctorTy) { () =>
              arguments().flatMap(apply(_))
            }
            .toList ++ List(() =>
            for {
              args <- arguments()
              newTarget <- synthesize(ctorTy)
              expr <- apply("undefined", args, newTarget)
            } yield expr,
          )
      shuffle(calls ++ constructs).iterator.flatMap(_()).nextOption()
    }

    private def apply(
      thisV: String,
      vs: List[String],
      newTarget: String,
    ): Option[String] =
      if (newTarget.isEmpty) { // without newTarget: XXX.call
        path match
          case BuiltinPath.Getter(base) =>
            descriptor(base).map(d => s"$d.get.call($thisV)")
          case BuiltinPath.Setter(base) =>
            val value = vs.headOption.getOrElse("undefined")
            descriptor(base).map(d => s"$d.set.call($thisV, $value)")
          case _ =>
            val args = (thisV :: vs).mkString(", ")
            access(path).map(fn => s"$fn.call($args)")
      } else { // with newTarget: Reflect.construct
        access(path).map { fn =>
          s"Reflect.construct($fn, [${vs.mkString(", ")}], $newTarget)"
        }
      }

    private def apply(vs: List[String]): Option[String] =
      access(path).map(fn => s"new ($fn)(${vs.mkString(", ")})")
  }

  private def isConstructable(func: Func, cfg: CFG): Boolean =
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
