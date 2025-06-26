package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.tychecker.*
import esmeta.cfg.*
import esmeta.ir.{IRElem, LangEdge}
import esmeta.state.*
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

trait StringifierDecl { self: Self =>

  /** stringifier for analyzer */
  class Stringifier(
    detail: Boolean,
    location: Boolean,
    asite: Boolean,
  ) {
    private val cfgStringifier = CFGElem.getStringifier(detail, location)
    import cfgStringifier.given

    private val irStringifier = IRElem.getStringifier(detail, location)
    import irStringifier.given

    /** elements */
    given Rule[View] = viewRule(detail)

    // control points
    given cpRule: Rule[ControlPoint] = (app, cp) =>
      app >> cp.func.name >> "[" >> cp.func.id >> "]:"
      app >> (cp match
        case NodePoint(_, node, view) => node.simpleString
        case ReturnPoint(func, view)  => "RETURN"
      )
      if (cp.view.isEmpty) app
      else app >> ":" >> cp.view

    given elemRule: Rule[AnalyzerElem] = (app, elem) =>
      elem match
        case elem: ControlPoint => cpRule(app, elem)
  }
}
