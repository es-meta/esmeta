package esmeta.cfg.util

import esmeta.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.{Appender, HtmlUtils}
import scala.collection.mutable.Queue

/** printer for Graphviz DOT format */
class DotPrinter(
  func: Func,
) {
  // helpers for functions
  lazy val funcId: String = s"cluster${func.id}"
  lazy val funcName: String = norm(func.headString)

  // helpers for function entry
  lazy val entryId: String = s"${funcId}_entry"
  lazy val entryColor: String = REACH
  lazy val entryBgColor: String = NORMAL
  lazy val entryEdgeColor: String = REACH

  // helpers for function exit
  lazy val exitId: String = s"${funcId}_exit"
  lazy val exitColor: String = REACH
  lazy val exitBgColor: String = NORMAL
  def exitEdgeColor(from: Node): String = REACH

  // helpers for nodes
  def getId(node: Node): String = s"node${node.id}"
  def getColor(node: Node): String = REACH
  def getBgColor(node: Node): String = NORMAL
  def getEdgeColor(from: Node, to: Node): String = REACH

  // colors
  def REACH = """"black""""
  def NON_REACH = """"gray""""
  def NORMAL = """"white""""
  def CURRENT = """"powderblue""""

  // functions
  def addTo(app: Appender): Appender =
    (app :> "subgraph " >> funcId >> " ").wrap {
      given Appender = app
      app :> "label = \"" >> funcName >> "\""
      app :> "style = rounded"
      val entry = func.entry
      var visited = Set[Node](entry)
      var queue = Queue[Node](entry)
      def add(nodeOpt: Option[Node]): Unit = for {
        node <- nodeOpt
        if !visited.contains(node)
        _ = queue.enqueue(node)
        _ = visited += node
      } ()
      drawEntry
      drawExit
      while (!queue.isEmpty)
        val cur = queue.dequeue
        drawNodeWithEdge(cur)
        cur match
          case block: Block   => add(block.next)
          case call: Call     => add(call.next)
          case branch: Branch => add(branch.thenNode); add(branch.elseNode)
    }

  // ---------------------------------------------------------------------------
  // protected helper functions
  // ---------------------------------------------------------------------------
  protected def drawEntry(using Appender): Unit =
    drawNaming(entryId, entryColor, "Entry")
    drawNode(entryId, "circle", entryColor, entryBgColor, None)
    drawEdge(entryId, getId(func.entry), entryEdgeColor, None)

  protected def drawExit(using Appender): Unit =
    drawNaming(exitId, exitColor, "Exit")
    drawNode(exitId, "circle", exitColor, exitBgColor, None)

  protected def drawNodeWithEdge(node: Node)(using Appender): Unit =
    val id = getId(node)
    val nodeColor = getColor(node)
    val bgColor = getBgColor(node)
    val edgeColor = getEdgeColor(node, node)
    drawNaming(id, nodeColor, node.simpleString)
    node match
      case Block(_, insts, nextOpt) =>
        drawNode(id, "box", nodeColor, bgColor, Some(norm(insts)))
        nextOpt match
          case Some(next) =>
            drawEdge(id, getId(next), getEdgeColor(node, next), None)
          case None =>
            val edgeColor = exitEdgeColor(node)
            drawEdge(id.toString, exitId, edgeColor, None)
      case Call(_, callInst, nextOpt) =>
        val simpleString = callInst match
          case ICall(lhs, fexpr, args) =>
            s"${norm(lhs)} = ${norm(fexpr)}(${args.map(norm(_)).mkString(", ")})"
          case IMethodCall(lhs, base, method, args) =>
            s"${norm(lhs)} = ${norm(base)}-&gt;${norm(method)}(${args.map(norm(_)).mkString(", ")})"
          case ISdoCall(lhs, ast, method, args) =>
            s"${norm(lhs)} = ${norm(ast)}-&gt;$method(${args.map(norm(_)).mkString(", ")})"
        drawNode(id, "cds", nodeColor, bgColor, Some(simpleString))
        nextOpt match
          case Some(next) =>
            drawEdge(id, getId(next), getEdgeColor(node, next), None)
          case None =>
            drawEdge(id.toString, exitId, edgeColor, None)
      case Branch(_, kind, cond, thenOpt, elseOpt) =>
        drawNode(id, "diamond", nodeColor, bgColor, Some(norm(cond)))
        thenOpt.map { thn =>
          drawEdge(id, getId(thn), getEdgeColor(node, thn), Some("true")),
        }
        elseOpt.map { els =>
          drawEdge(id, getId(els), getEdgeColor(node, els), Some("false")),
        }

  protected def drawNode(
    dotId: String,
    shape: String,
    color: String,
    bgColor: String,
    labelOpt: Option[String],
  )(using app: Appender): Unit = labelOpt match
    case Some(label) =>
      app :> s"""$dotId [shape=$shape, label=<<font color=$color>$label</font>> color=$color fillcolor=$bgColor, style=filled]"""
    case None =>
      app :> s"""$dotId [shape=$shape label=" " color=$color fillcolor=$bgColor style=filled]"""

  protected def drawEdge(
    fid: String,
    tid: String,
    color: String,
    labelOpt: Option[String],
  )(using app: Appender): Unit =
    app :> s"$fid -> $tid ["
    labelOpt.map { label =>
      app >> s"label=<<font color=$color>$label</font>> "
    }
    app >> s"color=$color]"

  protected def drawNaming(
    id: String,
    color: String,
    name: String,
  )(using app: Appender): Unit =
    app :> id >> "_name [shape=none, "
    app >> "label=<<font color=" >> color >> ">" >> name >> "</font>>]"
    app :> id >> "_name -> " >> id
    app >> " [arrowhead=none, color=" >> color >> ", style=dashed]"

  def norm(str: String): String =
    HtmlUtils.escapeHtml(str).replaceAll("\u0000", "U+0000")
  def norm(node: IRElem): String =
    norm(node.toString(detail = false, location = false))
  protected def norm(insts: Iterable[Inst]): String = (for {
    (inst, idx) <- insts.zipWithIndex
    str = norm(inst)
  } yield s"""[$idx] $str<BR ALIGN="LEFT"/>""").mkString
}
