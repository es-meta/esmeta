package esmeta.verify

import esmeta.*
import scala.util.chaining.*

class Transform(entry: cfg.Func, builtCfg: cfg.CFG):
  val counter = new InvokeIdCounter

  def wrapped(entry: cfg.Func): (Set[Node], Map[Node, Set[Node]]) =
    val idMap = Map.from(entry.nodes.map(n => n -> counter.nextId))
    val nodes = idMap.map[Node] { case (n, i) => CFGNode(i, n) }
    val edges: Map[Node, Set[Node]] = for {
      (from, to) <- entry.succs
    } yield CFGNode(idMap(from), from) -> to.map[Node](n =>
      CFGNode(idMap(n), n),
    )
    (nodes.toSet, edges)

  lazy val result: OrderGraph =
    val (nodes, edges) = wrapped(entry)

    val checker = FindSdo(
      builtCfg,
      entry,
    )

    checker.analyze
    val result = checker.getResult(
      checker.NodePoint(
        func = entry,
        node = entry.nodes.head,
        view = checker.View(),
      ),
    )

    println(
      s"Transform: ${entry.name} -> ${result}",
    )

    new OrderGraph(nodes, edges)

end Transform

object Transform:
// def apply(): Transform = new Transform()
end Transform
