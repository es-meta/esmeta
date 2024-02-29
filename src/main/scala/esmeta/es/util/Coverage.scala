package esmeta.es.util

import esmeta.cfg.*
import esmeta.interpreter.*
import esmeta.ir.*
import esmeta.es.*
import esmeta.test262.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Set => MSet, Map => MMap, ArrayBuffer}
import io.circe.*, io.circe.syntax.*

/** coverage measurement of cfg */
case class Coverage(
  cfg: CFG,
  test262: Option[Test262] = None,
  timeLimit: Option[Int] = None,
) {

  /** data structures for coverage */
  private lazy val programs: ArrayBuffer[(String, Int)] = ArrayBuffer()
  private lazy val nodeMap: MMap[Int, Int] = MMap()

  /** update coverage for a given ECMAScript program */
  private lazy val scriptParser = cfg.scriptParser
  def runWithSrc(src: String): State = {
    val script = scriptParser.from(src)
    // program infos
    val pid = programs.size
    val code = script.toString(grammar = Some(cfg.grammar)).trim
    val programSize = code.length
    programs += (("tmp", programSize))

    var markedAst = script.nodeSet

    // run interpreter and record touched
    val touched: MSet[Int] = MSet()
    val finalSt = new Interpreter(
      Initialize(cfg, code, Some(script)),
      timeLimit = timeLimit,
    ) {
      // check if current state need to be recorded
      private def needRecord: Boolean =
        val contexts = st.context :: st.callStack.map(_.context)
        val astOpt = contexts.flatMap(_.astOpt).headOption
        astOpt.fold(false)(markedAst contains _)

      // override eval for node
      override def eval(node: Node): Unit =
        // record touched
        if (needRecord) touched += node.id
        super.eval(node)

      // handle dynamically created ast
      override def eval(expr: Expr): Value =
        val v = super.eval(expr)
        (expr, v) match
          case (_: EParse, AstValue(ast)) if needRecord =>
            markedAst ++= ast.nodeSet
          case _ => /* do nothing */
        v
    }.result

    // update coverage
    for { nid <- touched } {
      nodeMap.get(nid) match
        case Some(pid0) if programs(pid0)._2 <= programSize => /* do nothing */
        case _ => nodeMap += (nid -> pid)
    }

    finalSt
  }

  def run(path: String): State = {
    // script AST
    val script = test262 match
      case Some(test262) => test262.loadTest(path)
      case None          => scriptParser.fromFile(path)

    // program name
    val programName = test262 match
      case Some(test262) => path
      case None          => path.split("/").reverse.head

    // program infos
    val pid = programs.size
    val code = script.toString(grammar = Some(cfg.grammar)).trim
    val programSize = code.length
    programs += ((programName, programSize))

    var markedAst = script.nodeSet

    // run interpreter and record touched
    val touched: MSet[Int] = MSet()
    val finalSt = new Interpreter(
      Initialize(cfg, code, Some(script)),
      timeLimit = timeLimit,
    ) {
      // check if current state need to be recorded
      private def needRecord: Boolean =
        val contexts = st.context :: st.callStack.map(_.context)
        val astOpt = contexts.flatMap(_.astOpt).headOption
        astOpt.fold(false)(markedAst contains _)

      // override eval for node
      override def eval(node: Node): Unit =
        // record touched
        if (needRecord) touched += node.id
        super.eval(node)

      // handle dynamically created ast
      override def eval(expr: Expr): Value =
        val v = super.eval(expr)
        (expr, v) match
          case (_: EParse, AstValue(ast)) if needRecord =>
            markedAst ++= ast.nodeSet
          case _ => /* do nothing */
        v
    }.result

    // update coverage
    for { nid <- touched } {
      nodeMap.get(nid) match
        case Some(pid0) if programs(pid0)._2 <= programSize => /* do nothing */
        case _ => nodeMap += (nid -> pid)
    }

    finalSt
  }

  /** get node coverage */
  def nodeCov: (Int, Int) = (nodeMap.size, cfg.nodeMap.size)

  /** get branch coverage */
  // TODO handle return-if-abrupt
  def branchCov: (Int, Int) =
    val branches = cfg.nodeMap.values.collect { case br: Branch => br }
    val count = branches.foldLeft(0) {
      case (acc, Branch(bid, _, _, Some(thenNode), Some(elseNode))) =>
        nodeMap.get(bid) match
          case Some(_) =>
            val t = if (nodeMap contains thenNode.id) 1 else 0
            val e = if (nodeMap contains elseNode.id) 1 else 0
            acc + t + e
          case _ => acc
      case (acc, _) => acc
    }
    (count, branches.size * 2)

  /** dump results */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    val covData = for {
      nid <- cfg.nodeMap.keySet.toList.sorted
    } yield nodeMap.get(nid).map(programs(_)._1)
    val sizeData = (for {
      pid <- nodeMap.values.toSet
      (name, size) = programs(pid)
    } yield name -> size).toList
    dumpJson(
      JsonObject(
        "coverage" -> covData.asJson,
        "size" -> sizeData.asJson,
      ),
      s"$baseDir/coverage.json",
      noSpace = true,
    )

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String = {
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    val nPercent = percent(nCovered, nTotal)
    val bPercent = percent(bCovered, bTotal)
    f"""- coverage:
       |  - node: $nCovered%,d/$nTotal%,d ($nPercent%2.2f%%)
       |  - branch: $bCovered%,d/$bTotal%,d ($bPercent%2.2f%%)""".stripMargin
  }

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
}
