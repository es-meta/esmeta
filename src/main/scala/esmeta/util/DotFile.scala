package esmeta.util

import java.util.Locale.ROOT
import org.apache.commons.text.StringEscapeUtils

trait Dottable[T]:
  extension (x: T)
    def id: String
    def label: String
    def edgeLabel(y: T): String
    def subgraph: Option[String] = None
    def color: DotFile.Color
    def bgColor: DotFile.Color
    def shape: DotFile.Shape = DotFile.Shape.Ellipse
    final def dotDef: String =
      s"""$id [label=$label, color="$color", shape="$shape", style="filled", fillcolor="$bgColor"]"""
    final def edgeDef(y: T): String =
      s"""$id -> ${y.id} [label=${x.edgeLabel(y)}]"""

class DotFile[Node: Dottable](
  nodes: Seq[Node],
  edges: Seq[(Node, Node)],
) {

  override def toString: String =
    ({
      val (isSub, topLevel) = nodes
        .partition(_.subgraph.isDefined)
      val c1 = isSub
        .groupBy(_.subgraph)
        .map { case (sub, ns) => (sub.getOrElse(???), ns) }
        .map {
          case (sub, ns) =>
            s"""subgraph "cluster_$sub" {
             |  label = "$sub";
             |  ${ns.map(_.dotDef).mkString("\n")}
             |}""".stripMargin
        }
      val c2 = topLevel.map(_.dotDef)
      c1 ++ c2
    }
      ++
      edges.map {
        case (from, to) => from.edgeDef(to)
      })
      .mkString("digraph G {\n", "\n", "\n}")
}

object DotFile {

  trait AsProductName { self: Product =>
    override def toString(): String = productPrefix.toLowerCase(ROOT)
  }

  enum Shape extends AsProductName:
    case Box, Ellipse, Diamond

  enum Color extends AsProductName:
    case Black, White, Red, Green, Blue, Yellow, Cyan

}
