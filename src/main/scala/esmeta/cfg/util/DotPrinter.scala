package esmeta.cfg.util

import esmeta.*
import esmeta.cfg.*
import esmeta.ir.IRElem
import esmeta.util.{Appender, HtmlUtils}
import scala.collection.mutable.Queue

trait DotPrinter {
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
    val app = new Appender
    (app >> "digraph ").wrap {
      this(app)
    }
    app.toString
  }

  // functions
  def addFunc(func: Func, app: Appender): Unit = {
    val appEdge = new Appender
    val id = getId(func)
    val name = getName(func)
    (app :> s"subgraph $id ").wrap {
      appEdge.wrap("", "") {
        app :> s"""label = "$name"""" >> LINE_SEP
        app :> s"""style = rounded""" >> LINE_SEP
        func.entry.map { drawReachables(_, app) }
      }
    } >> LINE_SEP
  }

  def drawReachables(node: Node, app: Appender): Unit = {
    var visited = Set[Node](node)
    var queue = Queue[Node](node)
    def add(nodeOpt: Option[Node]): Unit = nodeOpt.map { node =>
      if (!visited.contains(node)) { queue.enqueue(node); visited += node }
    }
    while (!queue.isEmpty) {
      val curr = queue.dequeue
      drawNode(curr, app)
      curr match {
        case block: Block   => add(block.next)
        case call: Call     => add(call.next)
        case branch: Branch => add(branch.thenNode); add(branch.elseNode)
      }
    }
  }

  def drawNode(node: Node, app: Appender): Unit = {
    val id = getId(node)
    val nodeColor = getColor(node)
    val bgColor = getBgColor(node)
    app :> s"""${id}_name [shape=none, label=<<font color=$nodeColor>${node.simpleString}</font>>]""" >> LINE_SEP
    app :> s"""${id}_name -> $id [arrowhead=none, color=$nodeColor, style=dashed]""" >> LINE_SEP
    node match {
      case Block(_, insts, nextOpt) => {
        app :> s"""$id [shape=box label=<<font color=$nodeColor>${norm(
          insts,
        )}</font>> color=$nodeColor fillcolor=$bgColor style=filled]""" >> LINE_SEP
        nextOpt.map { next =>
          drawEdge(id, getId(next), getColor(node, next), None, app),
        }
      }
      case Call(_, lhs, fexpr, args, nextOpt) => {
        val simpleString =
          s"${norm(lhs)}=${norm(fexpr)}(${args.map(norm(_)).mkString(", ")})"
        app :> s"""$id [shape=cds label=<<font color=$nodeColor>${norm(
          simpleString,
        )})</font>> color=$nodeColor fillcolor=$bgColor style=filled]""" >> LINE_SEP

        nextOpt.map { next =>
          drawEdge(id, getId(next), getColor(node, next), None, app),
        }
      }
      case Branch(_, kind, cond, thenOpt, elseOpt) => {
        val simpleString = cond.toString(false)
        app :> s"""$id [shape=diamond label=<<font color=$nodeColor>${norm(
          cond,
        )}</font>> color=$nodeColor fillcolor=$bgColor style=filled]""" >> LINE_SEP

        thenOpt.map { thn =>
          drawEdge(id, getId(thn), getColor(node, thn), Some("true"), app),
        }
        elseOpt.map { els =>
          drawEdge(id, getId(els), getColor(node, els), Some("false"), app),
        }
      }
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
    app >> s"color=$color]" >> LINE_SEP
  }

  def norm(str: String): String = str
    .replaceAll("\u0000", "U+0000")
  def norm(node: IRElem): String = {
    norm(
      HtmlUtils
        .escapeHtml(node.toString(detail = false, location = false)),
    )
  }
  def norm(nodes: Iterable[IRElem]): String = {
    nodes.map(norm(_)).mkString("""<BR ALIGN="LEFT"/>""")
  }
}
