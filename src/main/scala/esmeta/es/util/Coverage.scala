package esmeta.es.util

import esmeta.cfg.*
import esmeta.interpreter.*
import esmeta.ir.{EReturnIfAbrupt, Expr, EParse}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.test262.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import esmeta.es.util.Coverage.Interp

import scala.collection.mutable.{Set => MSet, Map => MMap, ArrayBuffer}
import scala.math.Ordering.Implicits.seqOrdering

case class Script(code: String, name: String)

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

  def apply(node: Node): Map[View, Script] = nodeViewMap.getOrElse(node, Map())
  def getScript(nv: NodeView): Option[Script] = apply(nv.node).get(nv.view)

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[View, Script]] = Map()
  private var nodeViews: Set[NodeView] = Set()

  // script reference counter
  private var counter: Map[Script, Int] = Map()
  def size: Int = counter.size

  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(
    script: Script,
  ): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    check(script, interp)
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

    // update node coverage
    for ((NodeView(node, rawView), tempView) <- interp.touchedNodeViews)
      val view: View = rawView match
        case None    => None
        case Some(_) => ??? // TODO: feature-sensitive
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> tempView

      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    if (updated)
      _minimalInfo += script.name -> ScriptInfo(touchedNodeViews.keys)

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  // TODO handle return-if-abrupt
  // def branchCov: (Int, Int) =
  //   val branches = cfg.nodeMap.values.collect { case br: Branch => br }
  //   val count = branches.foldLeft(0) {
  //     case (acc, Branch(bid, _, _, Some(thenNode), Some(elseNode))) =>
  //       nodeMap.get(bid) match
  //         case Some(_) =>
  //           val t = if (nodeMap contains thenNode.id) 1 else 0
  //           val e = if (nodeMap contains elseNode.id) 1 else 0
  //           acc + t + e
  //         case _ => acc
  //     case (acc, _) => acc
  //   }
  //   (count, branches.size * 2)

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withMsg: Boolean = false,
  ): Unit =
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted

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

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      // app :> "- branch: " >> branchCov
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
        "node" -> nodeView.node.id.asJson, // replace with nodeView
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

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes
      touchedNodeViews += NodeView(node, getView(node)) -> getNearest
      super.eval(node)

    // handle dynamically created ast
    // override def eval(expr: Expr): Value =
    //   val v = super.eval(expr)
    //   (expr, v) match
    //     case (_: EParse, AstValue(ast)) if needRecord =>
    //       markedAst ++= ast.nodeSet
    //     case _ => /* do nothing */
    //   v

    private def getView(node: Node): View =
      // TODO: impl this
      None

    // impl nearest
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-information for each script */
  private case class ScriptInfo(
    // test: ConformTest, // TODO: fill this
    touchedNodeViews: Iterable[NodeView],
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

  case class Cond(elem: Branch | EReturnIfAbrupt, cond: Boolean) {
    def neg: Cond = copy(cond = !cond)
  }

  /** ordering of syntax-sensitive views */
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))

  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)

}
