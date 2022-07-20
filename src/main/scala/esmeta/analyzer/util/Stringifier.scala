package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

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

  // elements
  given elemRule: Rule[AnalyzerElem] = (app, elem) =>
    elem match
      case view: View        => viewRule(app, view)
      case cp: ControlPoint  => cpRule(app, cp)
      case av: AValue        => avRule(app, av)
      case aref: AbsRefValue => refRule(app, aref)

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
      case NodePoint(_, node, view) => app >> view >> ":" >> node.simpleString
      case ReturnPoint(func, view) =>
        app >> view >> ":RET:" >> func.name >> s"[${func.id}]"

  // values for analysis
  given avRule: Rule[AValue] = (app, av) =>
    av match
      case AComp(AConst("noraml"), v, _) =>
        app >> "N(" >> v >> ")"
      case AComp(ty, value, target) =>
        app >> "comp[" >> ty >> "/" >> target >> "]"
        app >> "(" >> value >> ")"
      case NamedLoc(name)     => app >> "#" >> name
      case AllocSite(k, view) => app >> "#" >> k >> ":" >> view
      case SubMapLoc(baseLoc) => app >> baseLoc >> ":SubMap"
      case AClo(func, _) =>
        app >> "clo<" >> func.irFunc.name >> ">"
      case ACont(target, _) =>
        app >> "cont<" >> target >> ">"
      case AAst(ast) =>
        app >> f"☊[${ast.name}]<${ast.idx}> @ 0x${ast.hashCode}%08x"
      case AGrammar(name, params) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "grammar<" >> name
        if (!params.isEmpty) app >> "[" >> params >> "]"
        app >> ">"
      case AMath(n)     => app >> n
      case AConst(name) => app >> "~" >> name >> "~"
      case ACodeUnit(c) => app >> c.toInt >> "cu"
      case ASimple(sv)  => app >> sv.toString

  // abstract reference values
  given refRule: Rule[AbsRefValue] = (app, ref) =>
    ref match
      case AbsRefId(id)           => app >> id
      case AbsRefProp(base, prop) => app >> base >> "[" >> prop >> "]"
}