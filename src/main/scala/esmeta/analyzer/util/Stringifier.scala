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

  // TODO view
  given viewRule: Rule[View] = (app, elem) => ???

  // control points
  given cpRule: Rule[ControlPoint] = (app, cp) =>
    cp match
      case NodePoint(node, view) => app >> view >> ":" >> node
      case ReturnPoint(func, view) =>
        app >> view >> ":RET:" >> func.name >> s"[${func.id}]"
}
