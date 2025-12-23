package esmeta.es.util

import esmeta.analyzer.paramflow.{ParamFlowAnalyzer, ParamKind}
import esmeta.{LINE_SEP, TEST262TEST_LOG_DIR}
import esmeta.cfg.{Func => CFGFunc, *}
import esmeta.injector.*
import esmeta.interpreter.*
import esmeta.ir.*
import esmeta.parser.AstFrom
import esmeta.spec.*
import esmeta.ty.{*, given}
import esmeta.error.NotSupported
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.Interp
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.immutable.BitSet
import java.util.Base64

/** coverage measurement of cfg */
case class Coverage(
  cfg: CFG,
  tyCheck: Boolean = false,
  kFs: Int = 0,
  cp: Boolean = false,
  timeLimit: Option[Int] = None,
  all: Boolean = false,
  isTargetNode: (Node, State) => Boolean = (_, _) => true,
  isTargetBranch: (Branch, State) => Boolean = (_, _) => true,
  analyzer: Option[ParamFlowAnalyzer] = None,
) {
  import Coverage.{*, given}

  val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[View, Set[Script]]] = Map()
  private var nodeViews: Set[NodeView] = Set()
  private var condViewMap: Map[Cond, Map[View, Set[Script]]] = Map()
  var condViews: Set[CondView] = Set() // temporal public for instrumentation

  // meta-info for -test262test:all-tests
  private val pathMap: Map[String, Int] = if (all) {
    readJson[Map[Int, String]](
      s"$TEST262TEST_LOG_DIR/test262IdToTest262.json",
    ).map(_.swap)
  } else Map()

  def apply(node: Node): Map[View, Set[Script]] =
    nodeViewMap.getOrElse(node, Map())
  def getScripts(nv: NodeView): Option[Set[Script]] =
    apply(nv.node).get(nv.view)
  def getScript(nv: NodeView): Option[Script] =
    getScripts(nv).flatMap(_.headOption)

  def apply(cond: Cond): Map[View, Set[Script]] =
    condViewMap.getOrElse(cond, Map())
  def getScripts(cv: CondView): Option[Set[Script]] =
    apply(cv.cond).get(cv.view)
  def getScript(cv: CondView): Option[Script] =
    getScripts(cv).flatMap(_.headOption)

  // script reference counter
  private var counter: Map[Script, Int] = Map()
  def size: Int = counter.size

  // target conditional branches
  private var _targetCondViews: Map[Cond, Map[View, Set[Target]]] = Map()
  def targetCondViews: Map[Cond, Map[View, Set[Target]]] = _targetCondViews

  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(
    script: Script,
    ast: Option[Ast] = None,
  ): (State, Boolean, Boolean) =
    val sourceText = script.code.toString
    val interp = run(
      sourceText,
      ast.getOrElse(scriptParser.from(sourceText)),
      Some(script.code),
      Some(script.name),
    )
    this.synchronized(check(script, interp))

  /** evaluate a given ECMAScript program */
  def run(code: Code): Interp =
    val sourceText = code.toString
    val ast = scriptParser.from(sourceText)
    run(sourceText, ast, Some(code), None)

  /** evaluate a given ECMAScript program */
  def run(
    sourceText: String,
    ast: Ast,
    code: Option[Code],
    name: Option[String],
  ): Interp =
    val initSt = cfg.init.from(
      sourceText,
      Some(ast),
      Some(code.getOrElse(Code.Normal(sourceText))),
      name,
    )
    val interp = Interp(
      initSt,
      tyCheck,
      kFs,
      cp,
      timeLimit,
      isTargetNode,
      isTargetBranch,
      analyzer,
    )
    interp.result; interp

  def check(script: Script, interp: Interp): (State, Boolean, Boolean) = {
    val Script(code, _, supported) = script
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeView, Option[Target]] = Map()
    var touchedCondViews: Map[CondView, Set[Target]] = Map()

    // update node coverage
    for ((nodeView, nearest) <- interp.touchedNodeViews)
      touchedNodeViews += nodeView -> nearest
      getScripts(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(scripts) =>
          if (all) { update(nodeView, script); updated = true }
          else {
            val originalScript = scripts.head
            if (
              (!originalScript.supported && supported) ||
              (originalScript.code.length > code.length)
            ) {
              update(nodeView, script); updated = true
              blockingScripts += originalScript
            } else {
              blockingScripts += script
            }
          }

    // update branch coverage
    for ((condView, targets) <- interp.touchedCondViews)
      touchedCondViews += condView -> targets
      getScripts(condView) match
        case None =>
          update(condView, targets, script); updated = true; covered = true
        case Some(scripts) =>
          if (all) { update(condView, targets, script); updated = true }
          else {
            val originalScript = scripts.head
            if (
              (!originalScript.supported && supported) ||
              (originalScript.code.length > code.length)
            ) {
              update(condView, targets, script); updated = true
              blockingScripts += originalScript
            } else {
              blockingScripts += script
            }
          }

    if (!all && updated)
      _minimalInfo += script.name -> ScriptInfo(
        // TODO ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)
  }

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
    withMsg: Boolean = false,
  ): Unit = {
    mkdir(baseDir)
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
    // TODO if (withScriptInfo)
    // TODO   dumpDir[(String, ScriptInfo)](
    // TODO     name = "minimal ECMAScript assertions",
    // TODO     iterable = _minimalInfo,
    // TODO     dirname = s"$baseDir/minimal-assertion",
    // TODO     getName = _._1,
    // TODO     getData = {
    // TODO       case (_, ScriptInfo(test, _, _)) =>
    // TODO         Yaml(
    // TODO           "tag" -> test.exitTag.toString,
    // TODO           "assertions" -> test.assertions.map(_.toString),
    // TODO         )
    // TODO     },
    // TODO     remove = true,
    // TODO   )
    // TODO   log("Dumped assertions")
    if (withTargetCondViews)
      dumpJson(
        name = "target conditional branches",
        data = (for {
          (cond, viewMap) <- _targetCondViews
          (view, _) <- viewMap
        } yield getCondViewsId(CondView(cond, view))).toSeq.sorted.asJson,
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
  }

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
  private def update(nodeView: NodeView, script: Script): Unit =
    val NodeView(node, view) = nodeView
    nodeViews += nodeView
    nodeViewMap += node -> updated(apply(node), view, script)

  // update mapping from conditional branches to scripts
  private def update(
    condView: CondView,
    targets: Set[Target],
    script: Script,
  ): Unit = {
    condViews += condView
    val CondView(cond, view) = condView

    // update target branches
    val neg = condView.neg
    cond.branch match
      case Branch(_, _, EBool(_), _, _, _) =>
      case _ if getScripts(neg).isDefined  => removeTargetCond(neg)
      case _                               => addTargetCond(condView, targets)

    condViewMap += cond -> updated(apply(cond), view, script)
  }

  // update mapping
  private def updated[View](
    map: Map[View, Set[Script]],
    view: View,
    script: Script,
  ): Map[View, Set[Script]] =
    if (!all) {
      // decrease counter of original script
      map.get(view).flatMap(_.headOption).foreach { origScript =>
        val count = counter(origScript) - 1
        counter += (origScript -> count)
        if (count == 0) {
          counter -= origScript
          _minimalScripts -= origScript
          _minimalInfo -= origScript.name
        }
      }
      // increase counter of new script
      _minimalScripts += script
      counter += script -> (counter.getOrElse(script, 0) + 1)
      map + (view -> Set(script))
    } else {
      map + (view -> (map.getOrElse(view, Set()) + script))
    }

  // add a cond to targetConds
  private def addTargetCond(cv: CondView, targets: Set[Target]): Unit =
    val CondView(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> targets)
    _targetCondViews += cond -> newViews

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondView): Unit =
    val CondView(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty) _targetCondViews -= cond
      else _targetCondViews += cond -> newViews
    }

  // get JSON for node coverage
  private def nodeViewInfos(ordered: List[NodeView]): List[NodeViewInfo] =
    for {
      (nodeView, idx) <- ordered.zipWithIndex
      scripts <- getScripts(nodeView)
    } yield
      if (all) NodeViewInfo(idx, nodeView, encode(scripts, pathMap))
      else NodeViewInfo(idx, nodeView, scripts.head.name)

  // get JSON for branch coverage
  private def condViewInfos(ordered: List[CondView]): List[CondViewInfo] =
    for {
      (condView, idx) <- ordered.zipWithIndex
      scripts <- getScripts(condView)
    } yield CondViewInfo(idx, condView, scripts.head.name)

  // get test encoding helper
  private def encode(
    scripts: Set[Script],
    pathMap: Map[String, Int],
  ): String =
    val bs = scripts
      .map(t => pathMap(t.name.split("/tests/test262/test/").last))
      .foldLeft(BitSet.empty)(_ + _)
    val hexString =
      bs.toBitMask.reverse.map(l => String.format("%016x", l)).mkString
    val base64 = Base64.getEncoder.encodeToString(
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray,
    )
    val compressed = base64
      .foldLeft(List.empty[(Char, Int)]) {
        case (acc, ch) =>
          acc match
            case (last, count) :: rest if last == ch =>
              (last, count + 1) :: rest
            case _ => (ch, 1) :: acc
      }
      .reverse
    val compressedStr = "@" + compressed.map {
      case (char, count) => s"$char.$count."
    }.mkString
    if compressedStr.length < base64.length then compressedStr else base64
}

object Coverage {
  class Interp(
    initSt: State,
    tyCheck: Boolean,
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
    isTargetNode: (Node, State) => Boolean,
    isTargetBranch: (Branch, State) => Boolean,
    analyzer: Option[ParamFlowAnalyzer] = None,
  ) extends Interpreter(initSt, tyCheck = tyCheck, timeLimit = timeLimit) {
    var touchedNodeViews: Map[NodeView, Option[Target]] = Map()
    var touchedCondViews: Map[CondView, Set[Target]] = Map()
    var supported = true

    def isTest262Test: Boolean = initSt.filename.exists(_.contains("test262"))

    // override eval for cursor
    override def eval(cursor: Cursor): Boolean = cursor match
      case NodeCursor(_, node, _) if !isTest262Test =>
        st.context.visited += node
        try { eval(node); true }
        catch { case _: NotSupported => supported = false; false }
      case _ => super.eval(cursor)

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes if it is a target node
      if (isTargetNode(node, st))
        touchedNodeViews += NodeView(node, getView(node)) -> getNearest
      super.eval(node)

    // get impact targets for a given expression in a given context
    private def getTargets(
      context: Context,
      callStack: List[CallContext],
      node: Node,
      expr: Expr,
    ): Set[Target] = {
      def next(param: String): Set[Target] = callStack match {
        case head :: tail =>
          val idx = context.func.params.map(_.lhs.name).indexOf(param)
          val cc = head.context
          val cursor = cc.cursor.asInstanceOf[NodeCursor]
          val callInst = cursor.node.asInstanceOf[Call].callInst
          val args = callInst match
            case ICall(_, _, args)          => args
            case ISdoCall(_, base, _, args) => base :: args
          val arg = args.lift(idx).getOrElse(EUndef())
          getTargets(cc, tail, cursor.node, arg)
        case Nil => Set()
      }
      for {
        an <- analyzer.toSet
        curNp = an.NodePoint(context.func, node, an.emptyView)
        absSt = an.getResult(curNp)
        (absV, _) = an.transfer.transfer(expr)(using curNp)(absSt)
        id = node.id
        param <- absV.params
        target <- context.func.head match {
          case Some(_: SyntaxDirectedOperationHead) =>
            import ParamKind.*
            given cfg: CFG = st.cfg
            param match
              case This => Target(context.astOpt).toSet
              case ThisIdx(k) =>
                Target(context.astOpt.flatMap(_.children.lift(k).flatten)).toSet
              case Named(name) => next(name)
          case Some(_: BuiltinHead) =>
            import Code.*
            import ParamKind.*
            import Target.*
            given asgmtExprParser: AstFrom =
              st.cfg.esParser("AssignmentExpression", List(true, false, false))
            st.sourceCode match
              case Some(builtin: Builtin) =>
                val targets = builtin.targetArgs.toSet
                param match
                  case This =>
                    targets.collect { case target: BuiltinThis => target }.toSet
                  case Named(name) =>
                    val idx = (for {
                      case addr: Addr <- st.locals.get(Name("__args__"))
                      case record: RecordObj <- st.heap.map.get(addr)
                      args = record.map.keys.toList
                    } yield args.indexOf(name)).getOrElse(-1)
                    targets.collect {
                      case target: BuiltinArg if target.idx == idx => target
                    }.toSet
                  case _ => Set()
              case s => Set()
          case _ => next(param.asInstanceOf[ParamKind.Named].name)
        }
      } yield target
    }

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch if it is a target branch
      if (isTargetBranch(branch, st))
        val cond = Cond(branch, b)
        val targets = getTargets(st.context, st.callStack, branch, branch.cond)
        touchedCondViews += CondView(cond, getView(cond)) -> targets
      super.moveBranch(branch, b)

    // get syntax-sensitive views
    private def getView(node: Node | Cond): View =
      val stack = st.context.featureStack.take(kFs)
      val path = if (cp) then Some(st.context.callPath) else None
      stack match {
        case Nil                  => None
        case feature :: enclosing => Some(enclosing, feature, path)
      }

    // get nearest target
    private def getNearest: Option[Target] = st.context.nearest
  }

  /** meta-information for each script */
  case class ScriptInfo(
    // TODO test: ConformTest,
    touchedNodeViews: Iterable[NodeView],
    touchedCondViews: Iterable[CondView],
  )

  /** syntax-sensitive view */
  type View = Option[(List[Feature], Feature, Option[CallPath])]
  private def stringOfView(view: View) = view.fold("") {
    case (enclosing, feature, path) =>
      s"@ $feature${enclosing.mkString("[", ", ", "]")}:${path.getOrElse("")}"
  }
  sealed trait NodeOrCondView(view: View) {}
  case class NodeView(node: Node, view: View) extends NodeOrCondView(view) {
    override def toString: String = node.simpleString + stringOfView(view)
  }

  case class CondView(cond: Cond, view: View) extends NodeOrCondView(view) {
    override def toString: String = cond.toString + stringOfView(view)
    def neg: CondView = copy(cond = cond.neg)
  }

  case class FuncView(func: CFGFunc, view: View) {
    override def toString: String = func.name + stringOfView(view)
  }

  // branch or reference to EReturnIfAbrupt with boolean values
  // `true` (`false`) denotes then- (else-) branch or abrupt (non-abrupt) value
  case class Cond(branch: Branch, cond: Boolean) {
    def neg: Cond = copy(cond = !cond)

    // get id
    inline def id: Int = branch.id

    // condition string
    inline def condString: String = if (cond) "T" else "F"

    // get loc
    inline def loc: Option[Loc] = branch.loc

    // string representation
    def simpleString: String = s"${branch.simpleString}:$condString"

    // conversion to string
    override def toString: String = simpleString
  }

  import scala.math.Ordering.Implicits.seqOrdering

  /** ordering of syntax-sensitive views */
  given Ordering[Feature] = Ordering.by(_.toString)
  given Ordering[CallPath] = Ordering.by(_.toString)
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))
  given Ordering[Cond] = Ordering.by(cond => cond.id)
  given Ordering[CondView] = Ordering.by(v => (v.cond, v.view))

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)
  case class CondViewInfo(index: Int, condView: CondView, script: String)

  case class CoverageConstructor(
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  )
}
