package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.injector.*
import esmeta.interpreter.*
import esmeta.ir.{EReturnIfAbrupt, Expr, EParse, EBool}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.CoverageSmall.Interp
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import io.circe.generic.auto._

import scala.math.Ordering.Implicits.seqOrdering

/** coverage measurement of cfg with small memory usage */
case class CoverageSmall(
  cfg: CFG,
  kFs: Int = 0,
  cp: Boolean = false,
  timeLimit: Option[Int] = None,
  logDir: Option[String] = None, // TODO: use this
) {
  import CoverageSmall.{*, given}

  val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[ViewSmall, Script]] = Map()
  private var nodeViews: Set[NodeViewSmall] = Set()
  private var condViewMap: Map[Cond, Map[ViewSmall, Script]] = Map()
  private var condViews: Set[CondViewSmall] = Set()

  def apply(node: Node): Map[ViewSmall, Script] = nodeViewMap.getOrElse(node, Map())
  def getScript(nv: NodeViewSmall): Option[Script] = apply(nv.node).get(nv.view)

  def apply(cond: Cond): Map[ViewSmall, Script] = condViewMap.getOrElse(cond, Map())
  def getScript(cv: CondViewSmall): Option[Script] = apply(cv.cond).get(cv.view)

  // script reference counter
  private var counter: Map[Script, Int] = Map()
  def size: Int = counter.size

  // target conditional branches
  private var _targetCondViews: Map[Cond, Map[ViewSmall, Option[Nearest]]] = Map()
  def targetCondViews: Map[Cond, Map[ViewSmall, Option[Nearest]]] = _targetCondViews

  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    this.synchronized(check(script, interp))
  }

  def runAndCheck(ast: Ast, name: String): (State, Boolean, Boolean) = {
    val code = ast.toString(grammar = Some(cfg.grammar))
    val interp = run(code)
    this.synchronized(check(Script(code, name), interp))
  }

  def runAndCheckWithBlocking(
    script: Script,
  ): (State, Boolean, Boolean, Set[Script], Set[NodeViewSmall], Set[CondViewSmall]) = {
    val interp = run(script.code)
    this.synchronized(checkWithBlocking(script, interp))
  }

  /** evaluate a given ECMAScript program */
  def run(code: String): Interp = {
    val initSt = cfg.init.from(code)
    val interp = Interp(initSt, kFs, cp, timeLimit)
    interp.result; interp
  }

  /** evaluate a given ECMAScript AST */
  def run(ast: Ast): Interp = {
    val initSt = cfg.init.from(ast)
    val interp = Interp(initSt, kFs, cp, timeLimit)
    interp.result; interp
  }

  def check(script: Script, interp: Interp): (State, Boolean, Boolean) =
    val Script(code, _) = script
    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeViewSmall, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondViewSmall, Option[Nearest]] = Map()

    // update node coverage
    for ((nodeView, nearest) <- interp.touchedNodeViews)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((condView, nearest) <- interp.touchedCondViews)
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
        ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)

  def checkWithBlocking(
    script: Script,
    interp: Interp,
  ): (State, Boolean, Boolean, Set[Script], Set[NodeViewSmall], Set[CondViewSmall]) =
    val Script(code, _) = script
    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty
    var coveredNodeViews = Set.empty[NodeViewSmall]
    var coveredCondViews = Set.empty[CondViewSmall]

    var touchedNodeViews: Map[NodeViewSmall, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondViewSmall, Option[Nearest]] = Map()

    // update node coverage
    for ((nodeView, nearest) <- interp.touchedNodeViews)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None =>
          update(nodeView, script); coveredNodeViews += nodeView;
          updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((condView, nearest) <- interp.touchedCondViews)
      touchedCondViews += condView -> nearest
      getScript(condView) match
        case None =>
          update(condView, nearest, script); coveredCondViews += condView;
          updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )

    (
      finalSt,
      updated,
      covered,
      blockingScripts,
      coveredNodeViews,
      coveredCondViews,
    )

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size
  def branchViewCov: Int = condViews.size

  /** dump results with detail */
  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withScriptInfo = true,
    withTargetCondViews = true,
    withUnreachableFuncs = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withScriptInfo: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    // TODO(@hyp3rflow): use this for ignoring dump messages
    withMsg: Boolean = false,
  ): Unit =
    mkdir(baseDir)
    // TODO(@tmdghks): optimize this part not to re-sort the list
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted
    lazy val getNodeViewsId = orderedNodeViews.zipWithIndex.toMap
    lazy val getCondViewsId = orderedCondViews.zipWithIndex.toMap
    dumpJson(
      CoverageConstructor(kFs, cp, timeLimit),
      s"$baseDir/constructor.json",
    )

    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1e9
    def log(msg: Any): Unit = if (withMsg) println(s"[${elapsedSec}s] $msg")

    dumpJson(
      name = "node coverage",
      data = nodeViewInfos(orderedNodeViews),
      filename = s"$baseDir/node-coverage.json",
      noSpace = false,
    )
    log("Dumped node coverage")
    dumpJson(
      name = "branch coverage",
      data = condViewInfos(orderedCondViews),
      filename = s"$baseDir/branch-coverage.json",
      noSpace = false,
    )
    log("Dumped branch coverage")
    if (withScripts)
      dumpDir[Script](
        name = "minimal ECMAScript programs",
        iterable = _minimalScripts,
        dirname = s"$baseDir/minimal",
        getName = _.name,
        getData = USE_STRICT + _.code + LINE_SEP,
        remove = true,
      )
      log("Dumped scripts")
    if (withScriptInfo)
      dumpDir[(String, ScriptInfo)](
        name = "minimal ECMAScript assertions",
        iterable = _minimalInfo,
        dirname = s"$baseDir/minimal-assertion",
        getName = _._1,
        getData = _._2.test.core, // TODO: dump this as json?
        remove = true,
      )
      log("Dumped assertions")
    if (withTargetCondViews)
      dumpJson(
        name = "target conditional branches",
        data = (for {
          (cond, viewMap) <- _targetCondViews
          (view, _) <- viewMap
        } yield getCondViewsId(CondViewSmall(cond, view))).toSeq.sorted.asJson,
        filename = s"$baseDir/target-conds.json",
        noSpace = false,
      )
      log("dumped target conds")
    if (withUnreachableFuncs)
      dumpFile(
        name = "unreachable functions",
        data = cfg.funcs
          .filter(f => !nodeViewMap.contains(f.entry))
          .map(_.name)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$baseDir/unreach-funcs",
      )
      log("dumped unreachable functions")

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      app :> "- branch: " >> branchCov
    }
    if (kFs > 0) (app :> "- sensitive coverage:").wrap("", "") {
      app :> "- node: " >> nodeViewCov
      app :> "- branch: " >> branchViewCov
    }
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
  private def update(nodeView: NodeViewSmall, script: Script): Unit =
    val NodeViewSmall(node, view) = nodeView
    nodeViews += nodeView
    nodeViewMap += node -> updated(apply(node), view, script)

  // update mapping from conditional branches to scripts
  private def update(
    condView: CondViewSmall,
    nearest: Option[Nearest],
    script: Script,
  ): Unit =
    condViews += condView
    val CondViewSmall(cond, view) = condView

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
  private def updated[ViewSmall](
    map: Map[ViewSmall, Script],
    view: ViewSmall,
    script: Script,
  ): Map[ViewSmall, Script] =
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
  private def addTargetCond(cv: CondViewSmall, nearest: Option[Nearest]): Unit =
    val CondViewSmall(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> nearest)
    _targetCondViews += cond -> newViews

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondViewSmall): Unit =
    val CondViewSmall(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty)
        _targetCondViews -= cond
      else
        _targetCondViews += cond -> (views - view)
    }

  // get JSON for node coverage
  private def nodeViewInfos(ordered: List[NodeViewSmall]): List[NodeViewInfo] =
    for {
      (nodeView, idx) <- ordered.zipWithIndex
      script <- getScript(nodeView)
    } yield NodeViewInfo(idx, nodeView, script.name)

  // get JSON for branch coverage
  private def condViewInfos(ordered: List[CondViewSmall]): List[CondViewInfo] =
    for {
      (condView, idx) <- ordered.zipWithIndex
      script <- getScript(condView)
    } yield CondViewInfo(idx, condView, script.name)
}

object CoverageSmall {
  class Interp(
    initSt: State,
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  ) extends Interpreter(initSt, timeLimit = timeLimit, keepProvenance = true) {
    var touchedNodeViews: Map[NodeViewSmall, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondViewSmall, Option[Nearest]] = Map()

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes
      touchedNodeViews += NodeViewSmall(node, getView(node)) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch
      val cond = Cond(branch, b)
      touchedCondViews += CondViewSmall(cond, getView(cond)) -> getNearest
      super.moveBranch(branch, b)

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      val cond = Cond(riaExpr.idRef, abrupt)

      touchedCondViews += CondViewSmall(cond, getView(cond)) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // get syntax-sensitive views
    private def getView(node: Node | Cond): ViewSmall =
      val stack = st.context.featureStack.take(kFs)
      val path = if (cp) then Some(st.context.callPath) else None
      stack match {
        case Nil                  => None
        case feature :: enclosing => Some(enclosing, feature, path)
      }

    // get location information
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-information for each script */
  case class ScriptInfo(
    test: ConformTest,
    touchedNodeViews: Iterable[NodeViewSmall],
    touchedCondViews: Iterable[CondViewSmall],
  )

  /** syntax-sensitive view */
  type ViewSmall = Option[(List[Feature], Feature, Option[CallPath])]
  private def stringOfView(view: ViewSmall) = view.fold("") {
    case (enclosing, feature, path) =>
      s"@ $feature${enclosing.mkString("[", ", ", "]")}:${path.getOrElse("")}"
  }
  sealed trait NodeOrCondViewSmall(view: ViewSmall) {}
  case class NodeViewSmall(node: Node, view: ViewSmall) extends NodeOrCondViewSmall(view) {
    override def toString: String = node.simpleString + stringOfView(view)
  }

  case class CondViewSmall(cond: Cond, view: ViewSmall) extends NodeOrCondViewSmall(view) {
    override def toString: String = cond.toString + stringOfView(view)
    def neg: CondViewSmall = copy(cond = cond.neg)
  }

  case class FuncView(func: Func, view: ViewSmall) {
    override def toString: String = func.name + stringOfView(view)
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
  given Ordering[Feature] = Ordering.by(_.toString)
  given Ordering[CallPath] = Ordering.by(_.toString)
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeViewSmall] = Ordering.by(v => (v.node, v.view))
  given Ordering[Cond] = Ordering.by(cond => (cond.kindString, cond.id))
  given Ordering[CondViewSmall] = Ordering.by(v => (v.cond, v.view))

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeViewSmall, script: String)
  case class CondViewInfo(index: Int, condView: CondViewSmall, script: String)

  case class CoverageConstructor(
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  )

  def fromLogSimpl(baseDir: String, cfg: CFG): Coverage =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    def readJsonHere[T](json: String)(using Decoder[T]) =
      readJson[T](s"$baseDir/$json")

    val con: CoverageConstructor = readJsonHere("constructor.json")
    val cov = new Coverage(cfg, con.kFs, con.cp, con.timeLimit)

    for {
      minimal <- listFiles(s"$baseDir/minimal")
      name = minimal.getName
      code = readFile(minimal.getPath).drop(USE_STRICT.length).strip
      script = Script(code, name)
    } {
      try {
        cov.runAndCheck(script)
      } catch {
        case e =>
          println(f"Error in $name%-12s: $e  :  $code")
      }
    }

    cov
}
