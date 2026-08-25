package esmeta.solver

import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.*
import esmeta.es.util.Coverage.Cond
import esmeta.ir.{Func => _, *}
import esmeta.spec.{BuiltinHead, ParamKind}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap, PriorityQueue, Queue}

class SymInterp(
  val tychecker: TyChecker,
  val synthesizer: TySynthesizer,
  val entryFunc: Func,
  val target: Cond,
  val timeLimit: Option[Int] = None,
  val detail: Boolean = false,
) extends Solver {
  import tychecker.*, monad.*, SymTy.*, Result.*

  // start time
  val startTime: Long = System.currentTimeMillis

  // target function
  lazy val targetFunc: Func = cfg.funcOf(target.branch)

  // main entry point of symbolic execution
  lazy val result: Option[Config] = nextCandidate

  // get the next candidate configuration
  def nextCandidate: Option[Config] = results.nextOption

  private lazy val results: Iterator[Config] =
    if (!isCandidate(entryFunc)) Iterator.empty
    else
      val first = Iterator.single(search(() => initialize(entryFunc)))
      val rest = Iterator.continually(search(() => unwrap(pop)))
      (first ++ rest).takeWhile(_.isDefined).flatten

  private def search(resume: () => Unit): Option[Config] =
    try {
      resume()
      while (true) step
      None
    } catch {
      case Found(config)      => Some(config)
      case NotFound | Timeout => None
    }

  // ---------------------------------------------------------------------------
  // symbolic execution state
  // ---------------------------------------------------------------------------
  // candidate functions
  inline def isCandidate(f: Func): Boolean = candidateFuncs.contains(f)
  private lazy val candidateFuncs: Set[Func] =
    SymInterp.candidateFuncs(entryFunc, targetFunc)(using cfg)
  // candidate nodes
  inline def isCandidate(n: Node): Boolean = candidateNodes.contains(n)
  private lazy val candidateNodes: Set[Node] =
    SymInterp.candidateNodes(entryFunc, target.branch)(using cfg)

  def timeout: Boolean = timeLimit.exists { limit =>
    val duration = System.currentTimeMillis - startTime
    duration >= limit.toLong * 1000L
  }

  // ---------------------------------------------------------------------------
  // symbolic execution configuration
  // ---------------------------------------------------------------------------
  // current node being executed
  var node: Node = entryFunc.builtinEntry.getOrElse(entryFunc.entry)
  // current abstract state
  var st: AbsState = AbsState.Bot
  // side of the branch condition (true for then, false for else)
  var conds: List[Cond] = Nil
  // call stack
  var calls: List[Call] = Nil
  // visited functions to avoid infinite exploration
  var funcs: Set[Func] = Set(entryFunc)
  // visited loops to avoid infinite exploration
  var loops: Set[Branch] = Set.empty

  // priority queue of configurations for backtracking
  private val configs = PriorityQueue[(Config, Double)]()(Ordering.by {
    case (config, score) => (score, elsePriority(config))
  })

  // symbolic execution of a node
  private def step: Unit = {
    // abort symbolic execution once the per-side time limit is exceeded
    if (timeout) throw Result.Timeout

    // log current configuration
    log("=" * 80)
    log(s"Executing node ${node.name}: $wrap")
    log(
      s"Backtrack queue: ${configs.clone().dequeueAll.map(_._1.node.name).mkString(", ")}",
    )
    log("-" * 80)
    log(s"$node @ ${cfg.funcOf(node).name}")

    given np: NodePoint[?] = NodePoint(cfg.funcOf(node), node, emptyView)
    if (!isCandidate(node) || st.isBottom) return unwrap(pop)
    node match
      case Block(_, insts, next) =>
        st = insts.foldLeft(st) {
          case (nextSt, _) if nextSt.isBottom => nextSt
          case (nextSt, inst)                 => transfer.transfer(inst)(nextSt)
        }
        next match
          case Some(next) => node = next
          case None       => unwrap(pop)
      case call: Call =>
        call.callInst match
          case ICall(_, fexpr @ EClo(f, Nil), args) =>
            pushCall(call, fexpr, args)
            val callee = cfg.fnameMap(f)
            // enter a candidate function
            if (isCandidate(callee) && !funcs.contains(callee)) {
              (for {
                vs <- join(args.map(transfer.transfer))
              } yield {
                given AbsState = st
                val params = callee.irFunc.params
                val vars: Set[Base] = st.locals.keySet.toSet
                val newLocals: Map[Local, AbsValue] = (for {
                  (param, arg) <- (params zip vs)
                } yield param.lhs -> arg.kill(vars, false)).toMap
                st = st.copy(
                  locals = newLocals,
                  constr = st.constr.onlySym,
                )
              })(st)
              node = callee.entry
              funcs += callee
              calls ::= call
            } else unwrap(pop)
          // use a summary
          case ICall(_, fexpr, args) =>
            pushCall(call, fexpr, args)
            unwrap(pop)
          case _ => unwrap(pop) // TODO: handle other calls
      case branch: Branch if target.branch == branch =>
        // reached the target branch, check the constraint
        st = refine(branch, target.cond)(st)
        if (check)
          // log found constraints
          import AbsState.constrMapRule
          log("=" * 80)
          log(s"FOUND: ${stringify(st.constrForSyms)(using constrMapRule)}")
          log("-" * 80)
          log(node)
          throw Found(wrap)
        else unwrap(pop)
      case branch @ Branch(_, kind, cond, _, thenNode, elseNode, _) =>
        // already visited this loop, skip it
        if (loops.contains(branch)) unwrap(pop)
        else {
          // first time visiting this loop, explore it
          if (branch.isLoop) loops += branch
          (for { v <- transfer.transfer(cond); newSt <- get } yield {
            def aux(to: Node, taken: Boolean): Config =
              val b = BoolT(taken)
              val takenSt = refine(v, b)(st)
              wrap.copy(node = to, state = takenSt).push(Cond(branch, taken))
            (thenNode, elseNode) match
              case (Some(t), Some(e)) =>
                push(aux(t, true)); push(aux(e, false)); unwrap(pop)
              case (Some(t), None) =>
                push(aux(t, true)); unwrap(pop)
              case (None, Some(e)) =>
                push(aux(e, false)); unwrap(pop)
              case (None, None) => unwrap(pop)
          })(st)
        }
  }

  private var _configs: List[Config] = Nil
  def pushCall(
    call: Call,
    fexpr: Expr,
    args: List[Expr],
  )(using np: NodePoint[?]): Unit = call.next.map { next =>
    given callerNp: NodePoint[Call] = np.copy(node = call)
    (for {
      fv <- transfer.transfer(fexpr)
      given AbsState <- get
      fty = fv.ty
      vs <- join(args.map(transfer.transfer))
      st <- get
      x = call.lhs
    } yield {
      _configs = Nil
      var retV = AbsValue.Bot
      var retConstr = TypeConstr.Bot
      fty.clo match
        case CloTopTy           => retV ⊔= AbsValue(AnyT)
        case CloArrowTy(_, ret) => retV ⊔= AbsValue(ret)
        case CloSetTy(names) =>
          for {
            fname <- names
            f <- cfg.fnameMap.get(fname)
            (v, constr) = pushCall(callerNp, f, st, vs, x, next)
          } { retV ⊔= v; retConstr ||= constr }
      fty.cont match
        case Inf => retV ⊔= AbsValue(AnyT)
        case Fin(fids) =>
          for {
            fid <- fty.cont.toIterable(stop = false)
            f <- cfg.funcMap.get(fid)
            (v, constr) = pushCall(callerNp, f, st, vs, x, next)
          } { retV ⊔= v; retConstr ||= constr }
      if (!retV.isBottom)
        push(
          wrap.copy(
            state = st.define(x, retV).copy(constr = retConstr),
            node = next,
          ),
        )
      push(_configs)
    })(st)
  }

  /** solver-side refiners */
  private lazy val manualRefiners: Map[String, transfer.Refiner] =
    transfer.manualRefiners + (
      "GetMethod" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val v = transfer.manualRefiners("GetMethod")(func, vs, retTy, st)
        vs(1).ty.getProperty.fold(v)(p => AbsValue(SProp(SSym(0), p), v.guard))
      },
    )

  /** handle calls */
  def pushCall(
    callerNp: NodePoint[Call],
    callee: Func,
    callerSt: AbsState,
    vs: List[AbsValue],
    x: Local,
    next: Node,
  ): (AbsValue, TypeConstr) = {
    given NodePoint[Call] = callerNp
    given AbsState = callerSt
    val call = callerNp.node
    val retTy = callee.retTy.ty.toValue
    (for {
      refiner <- manualRefiners.get(callee.name)
      v = refiner(callee, vs, retTy, callerSt)
      newV = instantiate(v, vs, callerNp, callerSt)
    } yield (newV, TypeConstr.Top)).getOrElse {
      val rp = ReturnPoint(callee, emptyView)
      val ret = getResult(rp)
      val AbsRet(_, noSym, syms) = ret
      for ((_, (v, constr)) <- syms) {
        val newConstr = instantiate(constr, vs, callerNp, callerSt)
        val newSt = transfer.refine(newConstr)(callerSt)
        val newV = instantiate(v, vs, callerNp, callerSt)
        _configs ::= wrap.copy(
          state = newSt.define(x, newV).copy(constr = newConstr),
          node = next,
        )
      }
      val (v, constr) = noSym
      (
        instantiate(v, vs, callerNp, callerSt),
        instantiate(constr, vs, callerNp, callerSt),
      )
    }
  }

  /** instantiation of return value */
  def instantiate(
    value: AbsValue,
    vs: List[AbsValue],
    callerNp: NodePoint[Call],
    callerSt: AbsState,
  ): AbsValue =
    given AbsState = callerSt
    val call = callerNp.node
    val map = vs.zipWithIndex.map {
      case (v, i) => i -> v
    }.toMap
    transfer.instantiate(value, map).lift

  /** instantiation of return value */
  def instantiate(
    constr: TypeConstr,
    vs: List[AbsValue],
    callerNp: NodePoint[Call],
    callerSt: AbsState,
  ): TypeConstr =
    given AbsState = callerSt
    val map = vs.zipWithIndex.map {
      case (v, i) => i -> v
    }.toMap
    transfer.instantiate(constr, map)

  // ---------------------------------------------------------------------------
  // helper functions for configuration manipulation
  // ---------------------------------------------------------------------------
  // initialize the configuration for the given function
  private def initialize(func: Func): Unit = {
    node = func.builtinEntry.getOrElse(func.entry)
    st = func.head match {
      // built-in functions
      case Some(h: BuiltinHead) =>
        import ParamKind.*
        // environment for built-in functions
        var locals = Map[Local, AbsValue](
          NAME_THIS -> AbsValue(SThis),
          NAME_ARGS_LIST -> AbsValue(SArgs),
          NAME_NEW_TARGET -> AbsValue(SNewTarget),
        )
        val ps = h.params.zipWithIndex
        for {
          (p, i) <- ps
          sty = if (p.kind == Variadic) SArgs else SSym(i)
        } {
          locals += Name(p.name) -> AbsValue(sty)
        }
        // symbolic environment for built-in functions
        val symEnv = Map(
          SThis.sym -> ESValueT,
          SArgs.sym -> ListT(ESValueT),
          SNewTarget.sym -> (ConstructorT || UndefT),
        ) ++ (for ((p, i) <- ps if p.kind != Variadic) yield {
          i -> ESValueT
        })
        AbsState(true, locals, symEnv, TypeConstr.Top)
      case _ => AbsState.Bot
    }
  }

  // get the current configuration
  def wrap: Config = Config(node, st, conds, funcs, calls, loops)
  def unwrap(config: Config): Unit = {
    node = config.node
    st = config.state
    conds = config.conds
    funcs = config.funcs
    calls = config.calls
    loops = config.loops
  }

  // push the current config and refine it using the branch condition and side
  def push(config: Config): Unit =
    val next = config
    if (isCandidate(next.node) && !next.state.isBottom)
      configs.enqueue(next -> configScore(next))
  def push(configs: List[Config]): Unit = configs.foreach(push)

  // pop the previous config and backtrack
  def pop: Config =
    if (configs.isEmpty) throw NotFound
    else configs.dequeue()._1

  // refine the current abstract state based on the branch condition and side
  def refine(branch: Branch, taken: Boolean)(using NodePoint[?]): Updater =
    for {
      v <- transfer.transfer(branch.cond)
      _ <- refine(v, BoolT(taken))
    } yield ()

  private def refine(v: AbsValue, ty: ValueTy)(using
    NodePoint[?],
  ): Updater = st =>
    import TargetType.*
    val dty = TargetType(ty)
    val vty = v.ty(using st)
    val constr = v.guard.derive(vty, dty.ty)
    if (vty distinct ty) AbsState.Bot
    else transfer.refine(constr)(st)

  // configuration of symbolic execution
  case class Config(
    node: Node,
    state: AbsState,
    conds: List[Cond],
    funcs: Set[Func],
    calls: List[Call],
    loops: Set[Branch],
  ) {
    def push(cond: Cond): Config = copy(conds = cond :: conds)
    override def toString: String = stringify(this)
  }

  private def configScore(config: Config): Double = 0.0 // FIXME: priority score

  private def elsePriority(config: Config): Int =
    config.conds.headOption match
      case Some(Cond(_, false)) => 1
      case _                    => 0

  given stateRule: Rule[Config] = (app, config) => {
    app.wrap {
      app :> s"Node: ${config.node.id}"
      app :> s"AbsState: " >> config.state
      app :> s"Conds: ${config.conds.map(_.toString).mkString(", ")}"
      app :> s"Funcs: ${config.funcs.toList.map(_.id).sorted.mkString(", ")}"
      app :> s"Calls: ${config.calls.map(_.id).mkString(", ")}"
      app :> s"Loops: ${config.loops.toList.map(_.id).sorted.mkString(", ")}"
    }
  }

  // found valid path and formula
  enum Result extends Exception:
    case Found(state: Config)
    case NotFound
    case Timeout

  // logging
  def log(msg: => Any): Unit = if (detail) println(msg)
}

object SymInterp {
  def apply(
    cfg: CFG,
    timeLimit: Option[Int] = None,
    detail: Boolean = false,
  ): SymInterpRunner = {
    val tyChecker = TyChecker(cfg, silent = true)
    tyChecker.analyze
    val synthesizer = TySynthesizer(cfg)
    SymInterpRunner(tyChecker, synthesizer, timeLimit, detail)
  }

  /** BFS from `func` over the reverse call graph, mapping each reached function
    * to its distance (the number of call edges) from `func`. Traversal records
    * functions in `stopAt` but does not explore their callers.
    */
  def reachingDists(
    func: Func,
    stopAt: Set[Func] = Set.empty,
  )(using cfg: CFG): Map[Func, Int] = {
    val dist = MMap(func -> 0)
    val queue = Queue(func)
    while (queue.nonEmpty) {
      val cur = queue.dequeue()
      val nextDist = dist(cur) + 1
      if (!stopAt.contains(cur))
        for {
          caller <- cfg.callerOf.getOrElse(cur, Set.empty)
          if !dist.contains(caller)
        } {
          dist(caller) = nextDist
          queue.enqueue(caller)
        }
    }
    dist.toMap
  }

  /** built-in entries reaching the given branch, mapped to their distance (the
    * number of call edges from the entry to the branch's function)
    */
  def findEntries(branch: Branch)(using cfg: CFG): Map[Func, Int] =
    val func = cfg.funcOf(branch)
    if (func.isBuiltin) Map(func -> 0)
    else reachingDists(func).filter(_._1.isBuiltin)

  /** built-in entries reaching the given branch, ordered from the closest to
    * the farthest
    */
  def sortedEntries(branch: Branch)(using cfg: CFG): List[Func] =
    findEntries(branch).toList.sortBy((f, d) => (d, f.id)).map(_._1)

  /** functions that may lie on a call path from `entry` to `target` (always
    * including `entry` itself)
    */
  def candidateFuncs(entry: Func, target: Func)(using cfg: CFG): Set[Func] =
    if (target == entry) Set(target)
    else reachingDists(target, stopAt = Set(entry)).keySet + entry

  /** nodes within candidate functions that can still reach the `target` branch
    */
  def candidateNodes(entry: Func, target: Branch)(using cfg: CFG): Set[Node] =
    val targetFunc = cfg.funcOf(target)
    val funcs = candidateFuncs(entry, targetFunc)
    for {
      func <- funcs + entry
      node <- computeCandidateNodes(func, funcs, targetFunc, target)
    } yield node

  private def computeCandidateNodes(
    func: Func,
    candidateFuncs: Set[Func],
    targetFunc: Func,
    target: Branch,
  )(using cfg: CFG): Set[Node] =
    if (func == targetFunc) func.reachingTo(target) // direct reachables
    else {
      // reachables to call sites that can reach the target
      val callTargets = for {
        callTarget <- func.nodes.collect { case c: Call => c }
        calleeName <- callTarget.callInst match
          case ICall(_, EClo(fn, _), _) => Some(fn)
          case _                        => None
        callee <- cfg.fnameMap.get(calleeName)
        if candidateFuncs.contains(callee)
      } yield callTarget
      callTargets.flatMap(func.reachingTo)
    }
}
case class SymInterpRunner(
  tyChecker: TyChecker,
  synthesizer: TySynthesizer,
  timeLimit: Option[Int] = None,
  detail: Boolean = false,
) {
  def apply(func: Func, cond: Cond): SymInterp =
    new SymInterp(tyChecker, synthesizer, func, cond, timeLimit, detail)
}
