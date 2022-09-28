package esmeta.cfg.util

import esmeta.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.{Appender, HtmlUtils}
import scala.collection.mutable.Queue

trait DotPrinter {
  // exit
  val isExit: Boolean

  // helpers
  def getId(func: Func): String
  def getId(node: Node): String
  def getName(func: Func): String
  def getColor(node: Node): String
  def getColor(from: Node, to: Node): String
  def getBgColor(node: Node): String
  def apply(app: Appender): Unit

  // colors
  val REACH = """"black""""
  val NON_REACH = """"gray""""
  val NORMAL = """"white""""
  val CURRENT = """"powderblue""""

  // conversion to string
  override def toString: String = {
    val app = Appender()
    (app >> "digraph ").wrap {
      app :> """graph [fontname = "helvetica"]"""
      app :> """node [fontname = "helvetica"]"""
      app :> """edge [fontname = "helvetica"]"""
      this(app)
    }
    app.toString
  }

  // functions
  def addFunc(func: Func, app: Appender): Unit = {
    val appEdge = Appender()
    val id = getId(func)
    val name = getName(func)
    (app :> s"subgraph $id ").wrap {
      appEdge.wrap("", "") {
        app :> s"""label = "$name""""
        app :> s"""style = rounded"""
        drawReachables(func.entry, id, app)
      }
    }
  }

  def drawReachables(node: Node, funcId: String, app: Appender): Unit = {
    var visited = Set[Node](node)
    var queue = Queue[Node](node)
    def add(nodeOpt: Option[Node]): Unit = nodeOpt.map { node =>
      if (!visited.contains(node)) { queue.enqueue(node); visited += node }
    }
    drawEntry(node, funcId, app)
    drawExit(node, funcId, app)
    while (!queue.isEmpty) {
      val curr = queue.dequeue
      drawCfgNode(curr, funcId, app)
      curr match {
        case block: Block   => add(block.next)
        case call: Call     => add(call.next)
        case branch: Branch => add(branch.thenNode); add(branch.elseNode)
      }
    }
  }

  def drawEntry(node: Node, funcId: String, app: Appender): Unit = {
    val nodeId = getId(node)
    val entryId = s"${funcId}_entry"
    val nodeColor = getColor(node)
    val edgeColor = getColor(node, node)
    // val bgColor = getBgColor(node)
    val bgColor = NORMAL
    drawNamingNode(entryId, nodeColor, "Entry", app)
    drawNamingEdge(entryId, nodeColor, app)
    drawNode(entryId, "circle", nodeColor, bgColor, None, app)
    drawEdge(entryId, nodeId, edgeColor, None, app)
  }

  def drawExit(node: Node, funcId: String, app: Appender): Unit = {
    val nodeId = getId(node)
    val exitId = s"${funcId}_exit"
    val nodeColor = getColor(node)
    val edgeColor = getColor(node, node)
    val bgColor = if (isExit) CURRENT else NORMAL
    drawNamingNode(exitId, nodeColor, "Exit", app)
    drawNamingEdge(exitId, nodeColor, app)
    drawNode(exitId, "circle", nodeColor, bgColor, None, app)
  }

  def drawCfgNode(node: Node, funcId: String, app: Appender): Unit = {
    val id = getId(node)
    val nodeColor = getColor(node)
    val edgeColor = getColor(node, node)
    val bgColor = getBgColor(node)
    drawNamingNode(id, nodeColor, node.simpleString, app)
    drawNamingEdge(id, nodeColor, app)
    node match {
      case Block(_, insts, nextOpt) => {
        drawNode(id, "box", nodeColor, bgColor, Some(norm(insts)), app)
        nextOpt match {
          case Some(next) =>
            drawEdge(id, getId(next), getColor(node, next), None, app)
          case None =>
            drawEdge(id.toString, s"${funcId}_exit", edgeColor, None, app)
        }
      }
      case Call(_, callInst, nextOpt) => {
        val simpleString = callInst match
          case ICall(lhs, fexpr, args) =>
            s"${norm(lhs)} = ${norm(fexpr)}(${args.map(norm(_)).mkString(", ")})"
          case IMethodCall(lhs, base, method, args) =>
            s"${norm(lhs)} = ${norm(base)}-&gt;${norm(method)}(${args.map(norm(_)).mkString(", ")})"
          case ISdoCall(lhs, ast, method, args) =>
            s"${norm(lhs)} = ${norm(ast)}-&gt;$method(${args.map(norm(_)).mkString(", ")})"
        drawNode(id, "cds", nodeColor, bgColor, Some(simpleString), app)
        nextOpt match {
          case Some(next) =>
            drawEdge(id, getId(next), getColor(node, next), None, app)
          case None =>
            drawEdge(id.toString, s"${funcId}_exit", edgeColor, None, app)
        }
      }
      case Branch(_, kind, cond, thenOpt, elseOpt) => {
        drawNode(id, "diamond", nodeColor, bgColor, Some(norm(cond)), app)
        thenOpt.map { thn =>
          drawEdge(id, getId(thn), getColor(node, thn), Some("true"), app),
        }
        elseOpt.map { els =>
          drawEdge(id, getId(els), getColor(node, els), Some("false"), app),
        }
      }
    }
  }

  def drawNode(
    dotId: String,
    shape: String,
    color: String,
    bgColor: String,
    labelOpt: Option[String],
    app: Appender,
  ): Unit = {
    labelOpt match {
      case Some(label) =>
        app :> s"""$dotId [shape=$shape, label=<<font color=$color>$label</font>> color=$color fillcolor=$bgColor, style=filled]"""
      case None =>
        app :> s"""$dotId [shape=$shape label=" " color=$color fillcolor=$bgColor style=filled]"""
    }
  }

  def drawEdge(
    fid: String,
    tid: String,
    color: String,
    labelOpt: Option[String],
    app: Appender,
  ): Unit = {
    app :> s"$fid -> $tid ["
    labelOpt.map { label =>
      app >> s"label=<<font color=$color>$label</font>> "
    }
    app >> s"color=$color]"
  }

  def drawNamingNode(
    id: String,
    color: String,
    name: String,
    app: Appender,
  ): Unit = {
    app :> s"""${id}_name [shape=none, label=<<font color=$color>$name</font>>]"""
  }

  def drawNamingEdge(
    id: String,
    color: String,
    app: Appender,
  ): Unit = {
    app :> s"""${id}_name -> $id [arrowhead=none, color=$color, style=dashed]"""
  }

  def norm(str: String): String = str
    .replaceAll("\u0000", "U+0000")
  def norm(node: IRElem): String = {
    norm(
      HtmlUtils
        .escapeHtml(node.toString(detail = false, location = false)),
    )
  }
  def norm(insts: Iterable[Inst]): String = (for {
    (inst, idx) <- insts.zipWithIndex
    str = norm(inst)
  } yield s"""[$idx] $str<BR ALIGN="LEFT"/>""").mkString
}
