package esmeta.es.util

import esmeta.{LINE_SEP, TEST262_TEST_DIR}
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
case class Coverage(cfg: CFG, timeLimit: Option[Long] = Some(TIMEOUT)) {

  /** data structures for coverage */
  private lazy val programs: ArrayBuffer[(String, Int)] = ArrayBuffer()
  private lazy val nodeMap: MMap[Int, Int] = MMap()

  /** update coverage for a given ECMAScript program */
  private lazy val scriptParser = cfg.esParser("Script")
  def run(path: String): State = {
    // parse
    val fromTest262 = path startsWith TEST262_TEST_DIR
    val script = scriptParser.fromFile(path)
    val scriptStr = script.toString(grammar = Some(cfg.grammar))

    // update program infos
    val pid = programs.size
    val programSize = scriptStr.length
    val programName =
      (if (fromTest262) path.substring(TEST262_TEST_DIR.length + 1)
       else path.split("/").reverse.head)
    programs += ((programName, programSize))

    // handle test262
    var markedAst = script.nodeSet
    val (sourceText, ast) =
      if (fromTest262) test262.loadTest(script, MetaData(path).includes)
      else (scriptStr, script)

    // run interp and record touched
    val touched: MSet[Int] = MSet()
    val interpreter =
      new Interpreter(Initialize(cfg, sourceText, Some(ast)), Nil, false) {
        // check if current state need to be recorded
        private def needRecord: Boolean =
          val contexts = st.context :: st.callStack.map(_.context)
          val astOpt = contexts.flatMap(_.astOpt).headOption
          astOpt.fold(false)(markedAst contains _)

        // override interp for node
        override def interp(node: Node): Unit =
          // record touched
          if (needRecord) touched += node.id
          super.interp(node)

        // handle dynamically created ast
        override def interp(expr: Expr): Value =
          val v = super.interp(expr)
          (expr, v) match
            case (_: EParse, AstValue(ast)) if needRecord =>
              markedAst ++= ast.nodeSet
            case _ => /* do nothing */
          v
      }
    val finalSt = timeout(interpreter.result, timeLimit)

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

  /** convertion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String = {
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    "coverage:" + LINE_SEP +
    f"- node: $nCovered%,d/$nTotal%,d (${percent(nCovered, nTotal)}%2.2f%%)" + LINE_SEP +
    f"- branch: $bCovered%,d/$bTotal%,d (${percent(bCovered, bTotal)}%2.2f%%)"
  }

  // ********************************************************************************
  // * helpers
  // ********************************************************************************
  /** test262 helper */
  private lazy val test262 = Test262(cfg.spec)

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
