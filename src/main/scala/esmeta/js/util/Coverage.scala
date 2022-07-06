package esmeta.js.util

import esmeta.{LINE_SEP, TIMEOUT}
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Set => MSet, Map => MMap, ArrayBuffer}

/** coverage measurement of cfg */
case class Coverage(cfg: CFG, timeLimit: Option[Long] = Some(TIMEOUT)) {

  /** data structures for coverage */
  private lazy val programs: ArrayBuffer[String] = ArrayBuffer()
  private lazy val programSize: MMap[Int, Int] = MMap()
  private lazy val nodeMap: MMap[Int, Int] = MMap()

  /** update coverage for a given JavaScript program */
  def run(path: String, fromTest262: Boolean = true): State = {
    // parse
    val script = cfg.jsParser("Script").fromFile(path)
    val scriptStr = script.toString(grammar = Some(cfg.grammar))

    // update program infos
    val pid = programs.size
    programSize += (pid -> scriptStr.length)
    programs += path

    // handle test262
    var markedAst = script.nodeSet
    val (sourceText, ast) =
      if (fromTest262) test262.loadTest(script, MetaData(path).includes)
      else (scriptStr, script)

    // run interp and record touched
    val touched: MSet[Int] = MSet()
    val interp = new Interp(Initialize(cfg, sourceText, Some(ast)), Nil) {
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
    val finalSt = timeout(interp.fixpoint, timeLimit)

    // update coverage
    for { nid <- touched } {
      nodeMap.get(nid) match
        case Some(pid0)
            if programSize(pid0) <= programSize(pid) => /* do nothing */
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
    dumpFile(this.toString, s"$baseDir/summary")
    dumpJson(
      for {
        nid <- cfg.nodeMap.keySet.toList.sorted
      } yield nodeMap.get(nid).map(programs(_)),
      s"$baseDir/coverage.json",
      noSpace = true,
    )

  /** convertion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String = {
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    f"- node coverage: $nCovered%,d/$nTotal%,d (${percent(nCovered, nTotal)}%2.2f%%)" + LINE_SEP +
    f"- branch coverage: $bCovered%,d/$bTotal%,d (${percent(bCovered, bTotal)}%2.2f%%)"
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
