package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for analyzer */
class Stringifier(
  detail: Boolean = false,
  location: Boolean = false,
  asite: Boolean = false, // XXX
) {
  private val cfgStringifier = CFGElem.getStringifier(detail, location)
  import cfgStringifier.given

  // elements
  given elemRule: Rule[AnalyzerElem] = (app, elem) =>
    elem match
      case view: View       => viewRule(app, view)
      case cp: ControlPoint => cpRule(app, cp)

  // view
  given viewRule: Rule[View] = (app, view) => {
    def ctxtStr(
      calls: List[String],
      loops: List[LoopCtxt],
    ): Appender = if (detail) {
      app >> calls.mkString("[call: ", ", ", "]")
      app >> loops
        .map { case LoopCtxt(loop, depth) => s"${loop.id}($depth)" }
        .mkString("[loop: ", ", ", "]")
    } else {
      app >> "[call: " >> calls.length >> "]"
      app >> "[loop: " >> loops.length >> "]"
    }

    // ir contexts
    ctxtStr(view.calls.map(_.id.toString), view.loops)

    // types
    app >> view.tys.map(_.name).mkString("[type: ", ", ", "]")
  }

  // control points
  given cpRule: Rule[ControlPoint] = (app, cp) =>
    cp match
      case NodePoint(node, view) => app >> view >> ":" >> node.simpleString
      case ReturnPoint(func, view) =>
        app >> view >> ":RET:" >> func.name >> s"[${func.id}]"
}
