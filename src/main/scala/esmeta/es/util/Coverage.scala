package esmeta.es.util

import esmeta.cfg.*
import esmeta.interpreter.*
import esmeta.ir.{EReturnIfAbrupt, Expr, EParse, EBool}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.Interp
import esmeta.test262.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*

import scala.math.Ordering.Implicits.seqOrdering

/** coverage measurement of cfg */
case class Coverage(
  cfg: CFG,
  test262: Option[Test262] = None,
  timeLimit: Option[Int] = None,
) {
  import Coverage.{*, given}

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[View, Script]] = Map()
  private var nodeViews: Set[NodeView] = Set()
  private var condViewMap: Map[Cond, Map[View, Script]] = Map()
  private var condViews: Set[CondView] = Set()

  def apply(node: Node): Map[View, Script] = nodeViewMap.getOrElse(node, Map())
  def getScript(nv: NodeView): Option[Script] = apply(nv.node).get(nv.view)

  def apply(cond: Cond): Map[View, Script] = condViewMap.getOrElse(cond, Map())
  def getScript(cv: CondView): Option[Script] = apply(cv.cond).get(cv.view)

  // script reference counter
  private var counter: Map[Script, Int] = Map()
  def size: Int = counter.size

  // target conditional branches
  private var _targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = Map()
  def targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = _targetCondViews

  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(
    script: Script,
  ): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    this.synchronized(check(script, interp))
  }

  /** evaluate a given ECMAScript program */
  def run(code: String): Interp = {
    val initSt = cfg.init.from(code)
    val interp = Interp(initSt, timeLimit)
    interp.result; interp
  }

  def check(script: Script, interp: Interp): (State, Boolean, Boolean) =
    val Script(code, _) = script
    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // update node coverage
    for ((NodeView(node, rawView), nearest) <- interp.touchedNodeViews)
      val view: View = rawView match
        case None    => None
        case Some(_) => ??? // TODO: feature-sensitive
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> nearest

      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((CondView(cond, rawView), nearest) <- interp.touchedCondViews)
      val view: View = rawView match
        case None    => None
        case Some(_) => ??? // TODO: feature-sensitive
      val condView = CondView(cond, view)
      touchedCondViews += condView -> nearest

      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size
  def branchViewCov: Int = condViews.size

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withMsg: Boolean = false,
  ): Unit =
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted

    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1e9
    def log(msg: Any): Unit = if (withMsg) println(s"[${elapsedSec}s] $msg")

    dumpJson(
      name = "node coverage",
      data = nodeViewMapJson(orderedNodeViews),
      filename = s"$baseDir/node-coverage.json",
      noSpace = false,
    )
    log("Dumped node coverage")
    dumpJson(
      name = "branch coverage",
      data = condViewMapJson(orderedCondViews),
      filename = s"$baseDir/branch-coverage.json",
      noSpace = false,
    )
    log("Dumped branch coverage")

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      app :> "- branch: " >> branchCov
    }
    // TODO: sensitive coverage
    app.toString

  /** extension for AST */
  extension (ast: Ast) {

    /** get all child nodes */
    def nodeSet: Set[Ast] =
      var nodes = Set(ast)
      ast match
        case Syntactic(_, _, _, cs) =>
          for {
            child <- cs.flatten
            childNodes = child.nodeSet
          } nodes ++= childNodes
        case _ => /* do nothing */
      nodes
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // update mapping from nodes to scripts
  private def update(nodeView: NodeView, script: Script): Unit =
    val NodeView(node, view) = nodeView
    nodeViews += nodeView
    nodeViewMap += node -> updated(apply(node), view, script)

  // update mapping from conditional branches to scripts
  private def update(
    condView: CondView,
    nearest: Option[Nearest],
    script: Script,
  ): Unit =
    condViews += condView
    val CondView(cond, view) = condView

    // update target branches
    val neg = condView.neg
    cond.elem match
      case _ if nearest.isEmpty                               =>
      case Branch(_, _, EBool(_), _, _)                       =>
      case b: Branch if b.isChildPresentCheck(cfg)            =>
      case ref: WeakUIdRef[EReturnIfAbrupt] if !ref.get.check =>
      case _ if getScript(neg).isDefined => removeTargetCond(neg)
      case _                             => addTargetCond(condView, nearest)

    condViewMap += cond -> updated(apply(cond), view, script)

  // update mapping
  private def updated[View](
    map: Map[View, Script],
    view: View,
    script: Script,
  ): Map[View, Script] =
    // decrease counter of original script
    for (origScript <- map.get(view)) {
      val count = counter(origScript) - 1
      counter += (origScript -> count)
      if (count == 0) {
        counter -= origScript
        _minimalScripts -= origScript
        _minimalInfo -= origScript.name
      }
    }
    // increse counter of new script
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map + (view -> script)

  // add a cond to targetConds
  private def addTargetCond(cv: CondView, nearest: Option[Nearest]): Unit =
    val CondView(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> nearest)
    _targetCondViews += cond -> newViews

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondView): Unit =
    val CondView(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty)
        _targetCondViews -= cond
      else
        _targetCondViews += cond -> (views - view)
    }

  // get JSON for node coverage
  private def nodeViewInfos(ordered: List[NodeView]): List[NodeViewInfo] =
    for {
      (nodeView, idx) <- ordered.zipWithIndex
      script <- getScript(nodeView)
    } yield NodeViewInfo(idx, nodeView, script.name)

  // ported from https://github.com/kaist-plrg/esmeta/commit/fc351874a846aba36ddd940d1a6eeff391572b07
  private def nodeViewMapJson(ordered: List[NodeView]): Json =
    Json.fromValues(
      for {
        (nodeView, idx) <- ordered.zipWithIndex
        script <- getScript(nodeView)
      } yield Json.obj(
        "index" -> idx.asJson,
        "node" -> Json.obj(
          "func" -> cfg.funcOf(nodeView.node).name.asJson,
          "loc" -> nodeView.node.loc.map(_.toString).asJson,
        ),
        "script" -> script.name.asJson,
      ),
    )

  private def condViewMapJson(ordered: List[CondView]): Json =
    Json.fromValues(
      for {
        (condView, idx) <- ordered.zipWithIndex
        script <- getScript(condView)
      } yield Json.obj(
        "index" -> idx.asJson,
        "cond" -> Json.obj(
          "func" -> condView.cond.node.map(cfg.funcOf(_).name).asJson,
          "loc" -> condView.cond.loc.map(_.toString).asJson,
          "kind" -> condView.toString.asJson,
        ),
        "script" -> script.name.asJson,
      ),
    )

}

object Coverage {
  class Interp(
    initSt: State,
    timeLimit: Option[Int],
  ) extends Interpreter(initSt, timeLimit = timeLimit) {
    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes
      touchedNodeViews += NodeView(node, getView(node)) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch
      val cond = Cond(branch, b)
      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.moveBranch(branch, b)

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      val cond = Cond(riaExpr.idRef, abrupt)

      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // TODO: impl this
    private def getView(node: Node | Cond): View = None

    // get location information
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-information for each script */
  private case class ScriptInfo(
    // test: ConformTest, // TODO: fill this
    touchedNodeViews: Iterable[NodeView],
    touchedCondViews: Iterable[CondView],
  )

  // TODO: impl this
  type View = Option[(List[Int])]
  sealed trait NodeOrCondView(view: View) {}
  case class NodeView(node: Node, view: View) extends NodeOrCondView(view) {
    override def toString: String = node.simpleString
  }

  case class CondView(cond: Cond, view: View) extends NodeOrCondView(view) {
    def neg: CondView = copy(cond = cond.neg)
    override def toString: String = cond.toString
  }

  case class FuncView(func: Func, view: View) {
    override def toString: String = func.name
  }

  // branch or reference to EReturnIfAbrupt with boolean values
  // `true` (`false`) denotes then- (else-) branch or abrupt (non-abrupt) value
  case class Cond(elem: Branch | WeakUIdRef[EReturnIfAbrupt], cond: Boolean) {
    def neg: Cond = copy(cond = !cond)

    // short kind string
    def kindString: String = elem match
      case (branch: Branch)     => "Branch"
      case (ref: WeakUIdRef[_]) => "EReturnIfAbrupt"

    def shortKindString: String = kindString.take(1)

    // get id
    def id: Int = elem match
      case (branch: Branch)     => branch.id
      case (ref: WeakUIdRef[_]) => ref.id

    // condition string
    def condString: String = if (cond) "T" else "F"

    // get node
    def node: Option[Node] = elem match
      case branch: Branch                   => Some(branch)
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.cfgNode

    // get loc
    def loc: Option[Loc] = elem match
      case branch: Branch                   => branch.loc
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.loc

    // conversion to string
    override def toString: String = s"$kindString[$id]:$condString"

    def simpleString: String = s"$shortKindString[$id]:$condString"
  }

  /** ordering of syntax-sensitive views */
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))
  given Ordering[Cond] = Ordering.by(_.id)
  given Ordering[CondView] = Ordering.by(v => (v.cond, v.view))

  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)

}
