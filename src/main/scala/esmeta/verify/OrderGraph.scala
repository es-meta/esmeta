package esmeta.verify

import esmeta.*

sealed trait Node
case class Hole(invokeId: InvokeId, hole: es.Hole) extends Node
case class CFGNode(invokeId: InvokeId, node: cfg.Node) extends Node
case class Inlined(invokeId: InvokeId, og: OrderGraph) extends Node

class OrderGraph(
  val nodes: Set[Node],
  val edges: Map[Node, Set[Node]],
) {
  // Define the structure of the order graph
  // This could include nodes, edges, and any other necessary components
}
