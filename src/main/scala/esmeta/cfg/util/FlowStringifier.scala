package esmeta.cfg.util

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.Appender.*

/** stringify control flow information of CFG elements */
object FlowStringifier {

  def apply(elem: CFGElem): String = (new Appender >> elem).toString

  given cfgElem: Rule[CFGElem] = (app, elem) => {
    elem match
      case elem: CFG        => cfgRule(app, elem)
      case elem: Func       => funcRule(app, elem)
      case elem: Node       => nodeRule(app, elem)
      case elem: BranchKind => branchKindRule(app, elem)
  }

  given cfgRule: Rule[CFG] = (app, cfg) => {
    (app >> "CFG").wrap(" {", "}") {
      for (func <- cfg.funcs) app :> func
    }
  }

  given funcRule: Rule[Func] = (app, func) => {
    (app >> func.nameWithId).wrap(" {", "}") {
      for (node <- func.nodes.toList.sortBy(_.id)) app :> node
    }
  }

  given nodeRule: Rule[Node] = (app, node) => {
    app >> node.id
    val nexts = node.succs.map(_.id).toList.sorted
    if (nexts.nonEmpty) app >> " -> " >> nexts.mkString(", ")
    app
  }

  given branchKindRule: Rule[BranchKind] = (app, kind) => {
    app >> kind.toString
  }
}
