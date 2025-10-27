package esmeta.web

import esmeta.cfg.*
import esmeta.es.Ast
import esmeta.ir.{Func => IRFunc, *}
import esmeta.interpreter.Interpreter
import esmeta.lang.Syntax
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.state.*
import esmeta.util.Loc
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger extension of IR interpreter */
class Debugger(st: State) extends Interpreter(st) {
  import Debugger.*
  // ---------------------------------------------------------------------------
  // shortcuts
  // ---------------------------------------------------------------------------
  private inline def cfg = st.cfg
  private inline def cursor = st.context.cursor
  private inline def func = cursor.func
  private inline def irFunc = func.irFunc
  private inline def algoOpt: Option[Algorithm] = irFunc.algo

  // ---------------------------------------------------------------------------
  // overrides IR interpreter
  // ---------------------------------------------------------------------------
  // transition for node to more fine-grained execution within block node
  override def step: Boolean = {
    val ret = super.step
    cursor match {
      case _: ExitCursor if st.callStack.nonEmpty =>
        triggerBreaks
        saveBpCounts
      case _: ExitCursor =>
      case _: NodeCursor =>
    }
    ret
  }

  override def eval(node: Node): Unit = {
    saveBpCounts // save counter
    (cursor, node) match
      case (cursor: NodeCursor, block @ Block(_, insts, next)) =>
        val targetInst = insts(cursor.idx)
        countStep(cfg.funcOf(node).id, targetInst.loc)
        eval(targetInst)
        instCnt += 1
        if (cursor.idx == insts.length - 1) {
          st.context.moveNode
        } else {
          cursor.idx += 1
        }
      case _ =>
        instCnt += 1
        super.eval(node)
    triggerBreaks // trigger breakpoints
  }

  override def eval(expr: Expr): Value = expr match {
    case _: AllocExpr =>
      val addr = super.eval(expr)
      provenance._2 match {
        case Some(provAddr) if provAddr == addr => provenance = (stepCnt, None)
        case _                                  =>
      }
      addr
    case _ => super.eval(expr)
  }

  // ---------------------------------------------------------------------------
  // execution control
  // ---------------------------------------------------------------------------

  /** ToDo : eliminate unnecessary mutable variables */
  private var wasExited: Boolean = false
  private var provenance: (Int, Option[Addr]) = (0, None)

  var instCnt: Int = 0
  def getInstCnt: Int = instCnt

  // step until given predicate
  // TODO handle yet
  @tailrec
  final def stepWhile(
    pred: => Boolean,
    ignoreBreak: Boolean = false,
  ): StepResult =
    val (prevLoc, prevStackSize) = getSpecInfo
    val keep = step
    val (curLoc, curStackSize) = getSpecInfo

    wasExited =
      prevStackSize > curStackSize || (wasExited && prevLoc._2 == curLoc._2)
    val break = isBreaked && !ignoreBreak

    if (pred && keep && !break) stepWhile(pred, ignoreBreak)
    else if (break)
      StepResult.Breaked
    else if (keep)
      StepResult.Succeed
    else
      StepResult.Terminated

  final def stepUntil(
    pred: => Boolean,
    ignoreBreak: Boolean = false,
  ) = stepWhile(!pred, ignoreBreak)

  def stepExactly(
    count: Int,
    ignoreBreak: Boolean = false,
    fn: Option[() => Unit] = None,
  ): StepResult = {
    // XXX should throw exception if count is negative?
    if count <= 0 then
      return if stepCnt == 0 then StepResult.ReachedFront
      else StepResult.Succeed
    stepWhile(
      {
        fn.map(_())
        val current = stepCnt
        current < count
      },
      ignoreBreak,
    )
  }

  private def stepExactlyFrom(
    to: Int,
    ignoreBreak: Boolean = false,
    from: Option[Int] = None,
    fn: Option[() => Unit] = None,
  ): StepResult =
    reset
    from.foreach(stepExactly(_, ignoreBreak))
    stepExactly(to, ignoreBreak, fn)

  private def reset: Unit =
    val newDOpt = for {
      sourceText <- st.sourceText
    } yield Debugger(cfg.init.from(sourceText))
    val newD = newDOpt.get
    this.st.context = newD.st.context
    this.st.callStack = newD.st.callStack
    this.st.heap.map.clear()
    this.st.heap.map ++= newD.st.heap.map
    this.st.heap.size = newD.st.heap.size
    this.st.globals.clear()
    this.st.globals ++= newD.st.globals
    this.stepCnt = 0
    this.instCnt = 0

  // ir step
  final def irStep(opts: StepOptions) = {
    val (prevCursor, _) = getIrInfo
    stepUntil(
      {
        val (curCursor, _) = getIrInfo
        prevCursor != curCursor
      },
      opts.ignoreBreak,
    )
  }

  // ir step over
  final def irStepOver(opts: StepOptions) =
    val (prevCursor, prevStackSize) = getIrInfo
    stepUntil(
      {
        val (curCursor, stackSize) = getIrInfo
        prevCursor != curCursor && prevStackSize >= stackSize
      },
      opts.ignoreBreak,
    )

  // ir step over
  final def irStepOut(opts: StepOptions) =
    val (prevCursor, prevStackSize) = getIrInfo
    stepUntil(
      {
        val (curCursor, stackSize) = getIrInfo
        prevCursor != curCursor && prevStackSize > stackSize
      },
      opts.ignoreBreak,
    )

  // spec step
  final def specStep(opts: StepOptions) = {
    val (prevLoc, _) = getSpecInfo
    stepUntil(
      {
        (prevLoc._2.isEmpty ||
        prevLoc != getSpecInfo._1
        // ) && !isExitCursor
        )
      },
      opts.ignoreBreak,
    )
  }

  // spec step over
  final def specStepOver(opts: StepOptions) =
    val (prevLoc, prevStackSize) = getSpecInfo
    stepUntil(
      {
        val (loc, stackSize) = getSpecInfo
        (prevLoc._2.isEmpty || prevLoc != loc) &&
        (prevStackSize >= stackSize)
        // && !isExitCursor
      },
      opts.ignoreBreak,
    )

  // spec step out
  final def specStepOut(opts: StepOptions) =
    val (_, prevStackSize) = getSpecInfo
    stepUntil(
      {
        val (_, stackSize) = getSpecInfo
        (prevStackSize > stackSize) // && !isExitCursor
      },
      opts.ignoreBreak,
    )

  // spec step back
  final def specStepBack(opts: StepOptions) = {
    val ignoreBreak = opts.ignoreBreak
    val ((curLoc, curStackSize), curIter) = (getSpecInfo, stepCnt)

    var target: Int = 0
    var breakFlag: Boolean = false

    val calcTarget = () => {
      val (iterLoc, iterStackSize) = getSpecInfo
      val iterCond = curLoc._2.isDefined && curLoc == iterLoc
      if (!iterCond) {
        target = stepCnt
        breakpoints.foreach {
          case SpecBreakpoint(algoName, steps, true)
              if st.context.func.irFunc.algo
                .map(_.name) == Some(algoName) && iterLoc._2.contains(
                steps,
              ) =>
            breakFlag = true
          case _ => breakFlag = false
        }
      }
    }

    stepExactlyFrom(curIter - 1, true, fn = Some(calcTarget))
    val result = stepExactlyFrom(target, true)
    if (breakFlag && !ignoreBreak) StepResult.Breaked else result
  }

  // spec step back over
  final def specStepBackOver(opts: StepOptions) = {
    val ignoreBreak = opts.ignoreBreak
    val (curLoc, curStackSize) = getSpecInfo
    val currentIter = stepCnt

    var target: Int = 0
    val calcTarget = () => {
      val (iterLoc, iterStackSize) = getSpecInfo
      val cond =
        (curLoc._2.isDefined && curLoc == iterLoc) || (curStackSize < iterStackSize)
      if !cond then target = stepCnt
    }

    var breakFlag: Boolean = false
    val calcBp = () => {
      val (iterLoc, iterStackSize) = getSpecInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(algoName, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.irFunc.algo
              .map(_.name) == Some(algoName) && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = stepCnt
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcTarget))
    if (!ignoreBreak)
      stepExactlyFrom(
        currentIter - 1,
        true,
        from = Some(target),
        fn = Some(calcBp),
      )
    val result = stepExactlyFrom(target, true)
    if (breakFlag) StepResult.Breaked else result
  }

  // spec step back out
  final def specStepBackOut(opts: StepOptions) = {
    val ignoreBreak = opts.ignoreBreak
    val (curLoc, curStackSize) = getSpecInfo
    val currentIter = stepCnt

    var target: Int = 0
    var breakFlag: Boolean = false

    val calcTarget = () => {
      if !(curStackSize <= getSpecInfo._2) then target = stepCnt
    }

    val calcBp = () => {
      val (iterLoc, iterStackSize) = getSpecInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(algoName, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.irFunc.algo
              .map(_.name) == Some(algoName) && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = stepCnt
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcTarget))
    if (!ignoreBreak)
      stepExactlyFrom(
        currentIter - 1,
        true,
        from = Some(target),
        fn = Some(calcBp),
      )
    val result = stepExactlyFrom(target, true)
    if (breakFlag) StepResult.Breaked else result
  }

  // es step
  final def esAstStep(opts: StepOptions) =
    val (prevLoc, _) = getEsInfo
    stepUntil(
      {
        val (loc, _) = getEsInfo
        isEsEvaluation &&
        (prevLoc != loc) &&
        isAtFirst
      },
      opts.ignoreBreak,
    )

  // es step
  final def esStatementStep(opts: StepOptions) =
    val (prevLoc, _) = getEsInfo
    stepUntil(
      {
        val (loc, _) = getEsInfo
        isEsEvaluation &&
        isSingleStatementListItem &&
        isAtFirst
      },
      opts.ignoreBreak,
    )

  // es step over
  final def esStepOver(opts: StepOptions) =
    val (prevLoc, prevStackSize) = getEsInfo
    stepUntil(
      {
        val (loc, stackSize) = getEsInfo
        isEsEvaluation &&
        isSingleStatementListItem &&
        isAtFirst &&
        prevStackSize >= stackSize
      },
      opts.ignoreBreak,
    )

  // es step out
  final def esStepOut(opts: StepOptions) =
    val (_, prevStackSize) = getEsInfo
    stepUntil(
      {
        val (loc, stackSize) = getEsInfo
        isEsEvaluation &&
        isSingleStatementListItem &&
        isAtFirst &&
        prevStackSize > stackSize
      },
      opts.ignoreBreak,
    )

  // continue
  final def continue: StepResult = stepWhile { true }

  // rewind
  final def rewind: StepResult = {
    val (curLoc, curStackSize) = getSpecInfo
    val currentIter = stepCnt
    var target: Int = 0
    var breakFlag: Boolean = false

    val calcBp = () => {
      val (iterLoc, iterStackSize) = getSpecInfo
      val sameStep = (curLoc._2.isDefined && curLoc == iterLoc)
      breakpoints.foreach {
        case SpecBreakpoint(algoName, steps, enabled) if enabled && !sameStep =>
          if (
            st.context.func.irFunc.algo
              .map(_.name) == Some(algoName) && iterLoc._2.contains(
              steps,
            )
          )
            breakFlag = true
            target = stepCnt
        case _ =>
      }
    }

    stepExactlyFrom(currentIter - 1, true, fn = Some(calcBp))
    if (breakFlag)
      stepExactlyFrom(target, true)
      StepResult.Breaked
    else
      reset
      StepResult.Succeed
  }

  final def stepBackToProvenance(addr: Addr): StepResult = {
    provenance = (0, Some(addr))
    stepExactlyFrom(stepCnt, true)
    stepExactlyFrom(provenance._1, true)
  }

  final def stepCntPlus(opts: StepOptions) = {
    val tarstepCnt = stepCnt + 1
    stepUntil(
      {
        stepCnt == tarstepCnt
      },
      opts.ignoreBreak,
    )
  }

  final def stepCntMinus(opt: StepOptions) = {
    val targetIter = stepCnt - 1
    stepExactlyFrom(targetIter, true)
  }

  final def instCntPlus(opts: StepOptions) = {
    val target = getInstCnt + 1
    stepUntil(getInstCnt == target, opts.ignoreBreak)
  }

  final def instCntMinus(opt: StepOptions) = {
    val targetIter = getInstCnt - 1
    stepExactlyFrom(targetIter, true)
  }

  // ---------------------------------------------------------------------------
  // breakpoints
  // ---------------------------------------------------------------------------

  /** breakpoints used in debugger */
  trait Breakpoint {
    var enabled: Boolean
    private var prevCount = 0
    private var trigger = false
    def needTrigger: Boolean = {
      if (trigger) { trigger = false; true }
      else false
    }
    protected def on: Unit = trigger = true
    protected def count: Int
    def saveCount: Unit = prevCount = count
    def check: Unit = if (enabled && prevCount < count) this.on
    def toggle() = { enabled = !enabled }
    protected def getContexts: List[Context] =
      st.context :: st.callStack.map(_.context)
  }

  /** breakpoints by spec steps */
  case class SpecBreakpoint(
    algoName: String,
    steps: List[Int],
    var enabled: Boolean = true,
  ) extends Breakpoint {
    override protected def count: Int =
      getContexts.count(ctxt =>
        ctxt.func.irFunc.algo.map(_.name) == Some(
          algoName,
        ) && ctxt.cursor.stepsOpt == Some(steps),
      )
  }

  /** breakpoints by ECMAScript code line */
  case class EsBreakpoint(
    line: Int,
    var enabled: Boolean = true,
  ) extends Breakpoint {
    @tailrec
    private def isParent(ast: Ast, target: Ast): Boolean = ast.parent match
      case Some(parent) =>
        if (parent eq target) true
        else isParent(parent, target)
      case None => false

    // count the number of breakpoint target ast
    override protected def count: Int =
      val astList = getContexts.flatMap { ctxt =>
        ctxt.astOpt match
          case Some(ast) if ast.loc.isDefined =>
            val loc = ast.loc.get
            val (ls, le) = (loc.start.line, loc.end.line)
            if (ls != -1 && ls == le && ls == line) Some(ast)
            else None
          case _ => None
      }
      astList.filter(ast => astList.exists(isParent(_, ast))).length
  }

  // breakpoints
  private val breakpoints: ListBuffer[Breakpoint] = ListBuffer()
  private def disableBps(): List[Int] = {
    val enabledIndices = breakpoints.zipWithIndex.collect {
      case (bp, idx) if bp.enabled => idx
    }.toList
    breakpoints.foreach(_.enabled = false)
    enabledIndices
  }
  private def enableBps(enableIndices: List[Int]): Unit = {
    enableIndices.foreach(idx => breakpoints(idx).enabled = true)
  }
  protected def ignoreBps[T](action: => T): T = {
    val enabledIndices = disableBps()
    val result = action
    enableBps(enabledIndices)
    result
  }

  // trigger breakpoints
  private def triggerBreaks: Unit =
    for (b <- breakpoints) b.check

  // save counter for each breakpoint
  private def saveBpCounts: Unit =
    for (b <- breakpoints) b.saveCount

  // add breakpoints from serialized data
  final def addBreak(
    data: (Boolean, String, List[Int], Boolean),
  ): Boolean =
    val (_, algoName, steps, enabled) = data
    val bp = // TODO revert ESBreakpoint
      SpecBreakpoint(algoName, steps, enabled)

    val exists = (for {
      func <- cfg.funcs
      algo <- func.irFunc.algo
      if (algoName == algo.name)
    } yield for {
      node <- func.nodes
      loc <- node.loc
      if (loc.steps == steps)
    } yield {
      loc
    }).flatten.nonEmpty;

    if exists then
      breakpoints += bp
      true
    else false

  // remove breakpoint
  final def rmBreakAll: Unit = breakpoints.clear
  final def rmBreak(idx: Int): Unit = breakpoints.remove(idx)

  // toggle breakpoints
  final def toggleBreakAll: Unit = for { bp <- breakpoints } bp.toggle()
  final def toggleBreak(idx: Int): Unit = breakpoints(idx).toggle()

  // check if current step is in break
  private def isBreaked: Boolean = breakpoints.foldLeft(false) {
    case (acc, bp) => bp.needTrigger || acc
  }

  // ---------------------------------------------------------------------------
  // step auxiliaries
  // ---------------------------------------------------------------------------

  private def getIrInfo = (cursor, st.callStack.size)

  private def getSpecInfo = ((irFunc.name, cursor.stepsOpt), st.callStack.size)

  // es steps from ast span info
  private def getEsInfo =
    val ctxts = st.context :: st.callStack.map(_.context)
    val esCallStackSize = {
      val stackAddr = st(GLOBAL_EXECUTION_STACK).asAddr
      val stackSize = st.heap.map(stackAddr).size
      stackSize
    }
    val (ls, le) = ctxts.flatMap(_.esLocOpt).headOption.getOrElse((-1, -1))
    ((ls, le), esCallStackSize)

  /* check if function is .Evaluation of SDO */
  def isEsEvaluation =
    st.context.func.isSDO && st.context.name.endsWith("Evaluation")

  /* check if ast is single StatementListItem */
  def isSingleStatementListItem: Boolean = {
    // TODO should get these constants in a better way?
    val STATEMENT = "Statement"
    val DECLARATION = "Declaration"
    st.context.astOpt.flatMap(_.parent).map(_.name) match
      case Some(name) =>
        name == STATEMENT || name == DECLARATION
      case None => false
  }

  def isAtFirst = cursor match
    case NodeCursor(func, node, 0) => func.entry == node
    case _                         => false

  def isExitCursor = cursor match
    case _: ExitCursor => true
    case _             => false

  // ---------------------------------------------------------------------------
  // debugger info
  // ---------------------------------------------------------------------------

  extension (node: Node) {

    /** get location in spec of node */
    def stepsOpt: List[List[Int]] =

      def instList: List[Inst] = node match
        case Block(_, insts, _) => insts.toList
        case node: NodeWithInst => node.inst.fold(Nil)(_ :: Nil)
      def langOpt: List[Syntax] = instList.flatMap(_.langOpt)
      for {
        lang <- langOpt
        loc <- lang.loc
      } yield loc.steps

  }

  // XXX temporary info for debugging debugger itself
  def nodeStepsOpt(node: Node): List[List[Int]] = node.stepsOpt

  /** extension for cursor */
  extension (cursor: Cursor) {

    /** get ir instruction of current cursor */
    def instOpt: Option[Inst] = cursor match
      case NodeCursor(_, node, idx) =>
        node match
          case Block(_, insts, _) => Some(insts(idx))
          case node: NodeWithInst => node.inst
      case _: ExitCursor => None

    /** get syntax object of current cursor */
    def langOpt = cursor.instOpt.fold(None)(_.langOpt)

    /** get location in spec of current cursor */
    def stepsOpt: Option[List[Int]] = cursor.langOpt match
      case Some(lang) if lang.loc.isDefined => Some(lang.loc.get.steps)
      case _                                => None
  }

  /** extension for context */
  extension (ctxt: Context) {

    /** check if es call */
    def isEsCall: Boolean = ctxt.name == "Call" || ctxt.name == "Construct"

    /** location of ECMAScript code */
    def esLocOpt: Option[(Int, Int)] = ctxt.astOpt match
      case Some(ast) =>
        Some(ast.loc match
          case Some(loc) => (loc.start.offset, loc.end.offset)
          case None      => (-1, -1),
        )
      case None => None
  }

  /** extension for func */
  extension (func: Func) {

    /** check whether it is runtime SDO */
    def isRuntimeSDO: Boolean = func.irFunc.head match
      case Some(SyntaxDirectedOperationHead(_, _, false, _, _)) => true
      case _                                                    => false
  }

  /** heap information */
  def heapInfo = st.heap.map.map {
    case (addr, obj) =>
      import io.circe.*
      import io.circe.syntax.*
      (
        addr.toString,
        obj match
          case RecordObj(tname, map) =>
            Json.obj(
              "type" -> Json.fromString("RecordObj"),
              "tname" -> Json.fromString(tname),
              "map" -> Json.fromFields(map.map {
                case (k, v) => (k, v.toString.asJson)
              }),
              "stringform" -> Json.fromString(obj.toString),
            )
          case MapObj(map) =>
            Json.obj(
              "type" -> Json.fromString("MapObj"),
              "map" -> Json.fromFields(map.map {
                case (k, v) => (k.toString, v.toString.asJson)
              }),
              "stringform" -> Json.fromString(obj.toString),
            )
          case ListObj(values) =>
            Json.obj(
              "type" -> Json.fromString("ListObj"),
              "values" -> Json.arr((values.map(_.toString.asJson))*),
              "stringform" -> Json.fromString(obj.toString),
            )
          case YetObj(tname, msg) =>
            Json.obj(
              "type" -> Json.fromString("YetObj"),
              "tname" -> Json.fromString(tname),
              "msg" -> Json.fromString(msg),
              "stringform" -> Json.fromString(obj.toString),
            ),
      )
  }

  /** call stack information */
  def callStackInfo =
    def getInfo(
      c: Context,
      wasExited: Boolean = false,
    ) =
      val (nid, isExit) = c.cursor match
        case NodeCursor(_, n, _) => (n.id, false)
        case _: ExitCursor       => (-1, true)
      (
        c.func.id,
        // c.name,
        c.cursor.stepsOpt.getOrElse(List()),
        wasExited,
        (
          c.locals.collect {
            case (Name(name), v) => (name, v.toString)
          }.toList,
          c.retVal.toList.map { case (_, v) => v.toString },
        ),
        locally {
          val dynamic = c.visited
          val static = cfg.depGraph.deps(cfg.funcMap(c.func.id))
          // what to do?

          val currentNode = c.cursor match
            case NodeCursor(_, node, _) => Some(node)
            case ExitCursor(_)          => None

          val intersection = currentNode match
            case Some(node) => dynamic intersect static.getOrElse(node, Set())
            case None       => dynamic

          (intersection ++ currentNode).flatMap(_.stepsOpt).toList
        },
        func.toDot(nid, isExit),
        c.astOpt
          .flatMap(_.loc)
          .map(loc => (loc.start.offset, loc.end.offset))
          .getOrElse((-1, -1)),
      )

    getInfo(st.context, wasExited) :: st.callStack
      .map(_.context)
      .map(getInfo(_))

}

object Debugger {
  import esmeta.web.util.JsonProtocol as WebJsonProtocol
  import io.circe.*, io.circe.syntax.*

  /** step options */
  case class StepOptions(ignoreBreak: Boolean)

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed, ReachedFront

    def withAdditional(
      debugger: Debugger,
      reprint: Boolean = false,
    )(using WebJsonProtocol): Json =
      val webJsonProtocol = summon[WebJsonProtocol]
      import webJsonProtocol.given
      Json.fromFields(
        List(
          "result" -> this.ordinal.asJson,
          "callstack" -> debugger.callStackInfo.asJson,
          "stepCnt" -> debugger.getStepCnt.asJson,
          "instCnt" -> debugger.getInstCnt.asJson,
          "heap" -> debugger.heapInfo.asJson,
        ) ++ {
          if (reprint) then
            List(
              "reprint" -> debugger.st.sourceText.asJson,
              "ast" -> debugger.st.cachedAst.asJson,
            )
          else Nil
        },
      )
}
