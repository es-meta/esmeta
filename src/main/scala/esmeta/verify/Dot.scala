package esmeta.verify

extension (orderGraph: OrderGraph)

  private def toNodeDef(node: Node): String = {
    val id = node.hashCode()
    val label = node.toString.replace("\"", "\\\"") // 따옴표 이스케이프
    s"""  $id [label="$label"]"""
  }

  def toDot: String = {
    val sb = new StringBuilder
    sb.append("digraph OrderGraph {\n")
    sb.append("  node [shape=box, style=filled, fillcolor=lightgray];\n")

    // 노드 정의
    val nodes = orderGraph.nodes
    for (node <- nodes) do {
      sb.append(toNodeDef(node))
    }

    // 엣지 정의
    for {
      (from, toSet) <- orderGraph.edges
      to <- toSet
    } do {
      sb.append(s"  ${from.hashCode()} -> ${to.hashCode()};\n")
    }

    sb.append("}\n")
    sb.toString()
  }
end extension
