package esmeta.analyzer

import esmeta.cfg.{util => _, *}
import esmeta.error.*
import esmeta.error.NotSupported.given
import esmeta.es.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.{util => _, *}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.domain.*
import scala.annotation.tailrec

/** static analyzer */
abstract class Analyzer extends util.Decl with repl.Decl {

  // ---------------------------------------------------------------------------
  // Needs to be Implemented
  // ---------------------------------------------------------------------------
  /** control flow graph */
  val cfg: CFG

  /** worklist of control points */
  val worklist: Worklist[ControlPoint]

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean

  /** lookup for node points */
  def getResult(np: NodePoint[Node]): AbsState =
    npMap.getOrElse(np, AbsState.Bot)

  /** lookup for return points */
  def getResult(rp: ReturnPoint): AbsRet =
    rpMap.getOrElse(rp, AbsRet.Bot)

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String

  /** logging mode */
  val log: Boolean

  /** logging the current analysis result */
  def logging: Unit

  // ---------------------------------------------------------------------------
  // Abstract Domains
  // ---------------------------------------------------------------------------
  /** value domains */
  val AbsValue: ValueDomain
  type AbsValue <: Printable[AbsValue]
  trait ValueDomain extends Domain[AbsValue] {
    extension (elem: AbsValue) {

      /** get string of abstract value with an abstract state */
      def getString(state: AbsState): String
    }
  }

  /** state domains */
  val AbsState: StateDomain
  type AbsState <: Printable[AbsState]
  trait StateDomain extends Domain[AbsState] {
    extension (elem: AbsState) {

      /** has imprecise elements */
      def hasImprec: Boolean
    }
  }

  /** return value domains */
  val AbsRet: RetDomain
  type AbsRet <: Printable[AbsRet]
  trait RetDomain extends Domain[AbsRet] {
    extension (elem: AbsRet) {

      /** return value */
      def value: AbsValue
    }
  }

  // ---------------------------------------------------------------------------
  // Control Points
  // ---------------------------------------------------------------------------
  /** control points */
  sealed trait ControlPoint extends Printable[ControlPoint] {
    def view: View
    def func: Func
    def isBuiltin: Boolean = func.isBuiltin
    def toReturnPoint: ReturnPoint = this match
      case np: NodePoint[Node] => ReturnPoint(np.func, np.view)
      case rp: ReturnPoint     => rp
    def toEntryPoint: NodePoint[Node] = NodePoint(func, func.entry, view)
  }

  /** node points */
  case class NodePoint[+T <: Node](
    func: Func,
    node: T,
    view: View,
  ) extends ControlPoint {
    inline def noView: NodePoint[T] = copy(view = emptyView)
  }

  /** return points */
  case class ReturnPoint(
    func: Func,
    view: View,
  ) extends ControlPoint {
    inline def noView: ReturnPoint = copy(view = emptyView)
  }

  given Ordering[ControlPoint] = Ordering.by(_ match
    case NodePoint(f, n, _) => (f.id, n.id)
    case ReturnPoint(f, _)  => (f.id, Int.MaxValue),
  )

  given cpRule: Rule[ControlPoint] = (app, cp) => {
    app >> cp.func.name >> "[" >> cp.func.id >> "]:"
    app >> (cp match
      case NodePoint(_, node, view) => node.simpleString
      case ReturnPoint(func, view)  => "RETURN"
    )
    if (cp.view.isEmpty) app
    else app >> ":" >> cp.view
  }

  /** view abstraction for analysis sensitivities */
  type View <: ViewLike

  /** view abstraction for analysis sensitivities */
  trait ViewLike {

    /** empty check */
    def isEmpty: Boolean
  }

  /** appender */
  given viewRule: Rule[View]

  /** empty view */
  val emptyView: View

  /** get entry views of loops */
  def getEntryView(view: View): View

  // ---------------------------------------------------------------------------
  // Abstract Transfer Functions
  // ---------------------------------------------------------------------------
  /** abstract transfer function */
  type AbsTransfer <: AbsTransferLike
  val transfer: AbsTransfer

  /** abstract transfer function */
  trait AbsTransferLike {

    /** loading monads */
    import monad.*

    /** fixpiont computation */
    @tailrec
    final def fixpoint: Unit = worklist.next match {
      case Some(cp) =>
        // set the current control point
        curCp = Some(cp)
        // count how many visited for each control point
        count(cp)
        // increase iteration number
        iter += 1
        // check time limit
        if (iter % checkPeriod == 0) timeLimit.map(limit => {
          val duration = (System.currentTimeMillis - startTime) / 1000
          if (duration > limit) exploded("timeout")
        })
        // text-based debugging
        if (debugMode) println(s"${cp.func.name}:$cp")
        // run REPL
        if (useRepl) Repl(cp)
        // abstract transfer for the current control point
        else apply(cp)
        // keep going
        fixpoint
      case None =>
        // set the current control point
        curCp = None
        // finalize REPL
        if (useRepl) Repl.finished
    }

    /** transfer function for control points */
    def apply(cp: ControlPoint): Unit = cp match
      case (np: NodePoint[_]) => this(np)
      case (rp: ReturnPoint)  => this(rp)

    /** transfer function for node points */
    def apply(np: NodePoint[_]): Unit

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
      idx: Int,
    )(using np: NodePoint[_]): Updater

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue]

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue

    /** transfer for variadic operators */
    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue

    /** transfer for mathematical operators */
    def transfer(
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue
  }

  // ---------------------------------------------------------------------------
  // Analysis Results
  // ---------------------------------------------------------------------------
  /** abstract states in each node point */
  var npMap: Map[NodePoint[Node], AbsState] = Map()

  /** abstract states in each return point */
  var rpMap: Map[ReturnPoint, AbsRet] = Map()

  /** abstract states right before calling functions */
  var callInfo: Map[NodePoint[Call], AbsState] = Map()

  /** return edges */
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map()

  /** current control point */
  var curCp: Option[ControlPoint] = None

  /** the number of iterations */
  var iter: Int = 0

  /** count for each control point */
  var counter: Map[ControlPoint, Int] = Map()

  /** set start time of analyzer */
  var startTime: Long = System.currentTimeMillis

  /** analysis time limit */
  val timeLimit: Option[Long] = None

  /** debugging mode */
  val debugMode: Boolean = false

  /** REPL mode */
  val useRepl: Boolean = false

  /** Run continue command at startup when using repl */
  val replContinue: Boolean = false

  /** check period */
  val checkPeriod: Int = 10000

  /** throw exception for not yet compiled expressions */
  val yetThrow: Boolean = false

  /** detailed string */
  val detail: Boolean = false

  /** string with location */
  val location: Boolean = false

  /** stringifier for ES */
  val esStringifier = ESElem.getStringifier((detail, location, None))

  /** stringifier for states */
  val stateStringifier = StateElem.getStringifier((detail, location))

  /** stringifier for CFG */
  val cfgStringifier = CFGElem.getStringifier(detail, location)

  /** stringifier for CFG */
  val irStringifier = IRElem.getStringifier(detail, location)

  // ---------------------------------------------------------------------------
  // Predefined Definitions
  // ---------------------------------------------------------------------------
  given CFG = cfg

  /** get syntax-directed operation (SDO) */
  val getSdo = cached[(Ast, String), Option[(Ast, Func)]](_.getSdo(_))

  /** monad helper */
  val monad: StateMonad[AbsState] = StateMonad[AbsState]()

  /** RunJobs function */
  val runJobs = cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, emptyView)

  /** increase the counter */
  def count(cp: ControlPoint): Unit =
    counter += cp -> (counter.getOrElse(cp, 0) + 1)

  /** get elapsed time of analyzer */
  def elapsedTime: Long = System.currentTimeMillis - startTime

  /** set start time of analyzer */
  def allCPs: Set[ControlPoint] = npMap.keySet ++ rpMap.keySet

  /** set of analyzed functions */
  def analyzedFuncs: Set[Func] = npMap.keySet.map(_.func) ++ analyzedReturns

  /** set of analyzed nodes */
  def analyzedNodes: Set[Node] = npMap.keySet.map(_.node)

  /** set of analyzed function returns */
  def analyzedReturns: Set[Func] = rpMap.keySet.map(_.func)

  /** get string for result of all control points */
  def getStrings: List[String] = getStrings(None, false)
  def getStrings(
    color: Option[String] = None,
    detail: Boolean = false,
  ): List[String] = allCPs.toList.sorted.map(getString(_, color, detail))

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: String,
    detail: Boolean,
  ): String = getString(cp, Some(color), detail)

  /** not supported */
  def notSupported(msg: String): Nothing = throw NotSupported(msg)
}
