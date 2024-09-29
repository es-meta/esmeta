package esmeta.analyzer.util

import esmeta.analyzer.*
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

    import TyStringifier.given

    /** elements */
    given elemRule: Rule[AnalyzerElem] = (app, elem) =>
      elem match
        case elem: AnalysisPoint => apRule(app, elem)

    // analysis points
    given apRule: Rule[AnalysisPoint] = (app, ap) =>
      given Rule[IRElem with LangEdge] = addLocRule
      if (detail) ap match
        case tp: TypeErrorPoint => app >> tp.node.simpleString >> " "
        case _                  =>
      ap match
        case cp: ControlPoint => cpRule(app, cp)
        case CallPoint(callerNp, callee) =>
          app >> "function call from "
          app >> callerNp.func.name >> callerNp.node.callInst
          app >> " to " >> callee.name
        case aap @ ArgAssignPoint(cp, idx) =>
          val param = aap.param
          app >> "argument assignment to "
          app >> (idx + 1).toOrdinal >> " parameter _" >> param.lhs.name >> "_"
          app >> " when " >> cp
        case InternalReturnPoint(returnNp, irReturn) =>
          app >> "return statement in " >> returnNp.func.name >> irReturn
        case FieldBasePoint(fieldPoint) =>
          app >> "base in" >> fieldPoint
        case FieldPoint(cp, field) =>
          app >> "field lookup in " >> cp.func.name >> field
        case UnaryOpPoint(cp, unary) =>
          app >> "unary operation (" >> unary.uop >> ") in " >> cp.func.name
          app >> unary
        case BinaryOpPoint(cp, binary) =>
          app >> "binary operation (" >> binary.bop >> ") in " >> cp.func.name
          app >> binary

    // control points
    given cpRule: Rule[ControlPoint] = (app, cp) =>
      app >> cp.func.name >> "[" >> cp.func.id >> "]:"
      app >> (cp match
        case NodePoint(_, node, view) => node.simpleString
        case ReturnPoint(func, view)  => "RETURN"
      )
      if (cp.view.isEmpty) app
      else app >> ":" >> cp.view

    private val addLocRule: Rule[IRElem with LangEdge] = (app, elem) =>
      for {
        lang <- elem.langOpt
        loc <- lang.loc
      } app >> " " >> loc.toString
      app
  }
}
