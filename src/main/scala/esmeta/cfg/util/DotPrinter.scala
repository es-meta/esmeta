package esmeta.cfg.util

import esmeta.*
import esmeta.cfg.*
import esmeta.util.Appender

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
    val id = getId(func)
    val name = getName(func)
    (app :> s"subgraph $id ").wrap {
      app :> s"""label = "$name"""" >> LINE_SEP
      app :> s"""style = rounded""" >> LINE_SEP
      for (node <- func.nodes) addNode(node, app)
      ???
    }
  }

  // nodes
  def addNode(node: Node, app: Appender): Unit = {
    ???
  }

  // edges
  def addLinear(
    from: Node,
    to: Node,
    app: Appender,
  ): Unit = {
    ???
  }
  def addBranch(
    branch: Branch,
    thenNode: Node,
    elseNode: Node,
    app: Appender,
  ): Unit = {
    ???
  }

  def addEdge(
    fid: String,
    tid: String,
    color: String,
    labelOpt: Option[String],
    app: Appender,
  ): Unit = {
    ???
  }
}
