package esmeta.cfg

import esmeta.cfg.util.*
import scala.collection.mutable.ListBuffer

// control-flow graphs (CFGs)
case class CFG(
  main: Func,
  funcs: ListBuffer[Func],
) extends CFGElem {
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap
  lazy val fnameMap: Map[String, Func] =
    (for (func <- funcs) yield func.ir.name -> func).toMap
  lazy val nodeMap: Map[Int, Node] = (for {
    func <- funcs
    node <- func.nodes
  } yield node.id -> node).toMap
  lazy val funcOf: Map[Node, Func] = (for {
    func <- funcs
    node <- func.nodes
  } yield node -> func).toMap
}
