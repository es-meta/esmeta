package esmeta.ai.util

import esmeta.ai.Config.*
import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ir.IRElem
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.state.SimpleValue

/** stringifier for ai */
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
  given elemRule: Rule[AnalyzerElem] = (app, elem) =>
    elem match
      case view: View       => viewRule(app, view)
      case cp: ControlPoint => cpRule(app, cp)
      case av: AValue       => avRule(app, av)
  // TODO case ty: Type          => typeRule(app, ty)

  /** view */
  given viewRule: Rule[View] = (app, view) =>
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
    if (IR_SENS) ctxtStr(view.calls.map(_.id.toString), view.loops)
    // TODO type contexts
    // if (TYPE_SENS) {
    //   given Rule[Iterable[Type]] = iterableRule("[", ", ", "]")
    //   app >> view.tys
    // }
    app

  // control points
  given cpRule: Rule[ControlPoint] = (app, cp) =>
    cp match
      case NodePoint(_, node, view) => app >> view >> ":" >> node.simpleString
      case ReturnPoint(func, view) =>
        app >> view >> ":RET:" >> func.name >> s"[${func.id}]"

  // values for analysis
  given avRule: Rule[AValue] = (app, av) =>
    av match
      case AComp(Const("noraml"), v, _) =>
        app >> "N(" >> v >> ")"
      case AComp(ty, value, target) =>
        app >> "comp[" >> ty
        app >> "/" >> target.getOrElse(CONST_EMPTY.toString) >> "]"
        app >> "(" >> value >> ")"
      case Named(name)        => app >> "#" >> name
      case AllocSite(k, view) => app >> "#" >> k >> ":" >> view
      case SubMap(baseLoc)    => app >> baseLoc >> ":SubMap"
      case AClo(func, _) =>
        app >> "clo<" >> func.irFunc.name >> ">"
      case ACont(target, _) =>
        app >> "cont<" >> target >> ">"
      case AstValue(ast) =>
        app >> f"☊[${ast.name}]<${ast.idx}> @ 0x${ast.hashCode}%08x"
      case Grammar(name, params) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "grammar<" >> name
        if (!params.isEmpty) app >> "[" >> params >> "]"
        app >> ">"
      case Math(n)         => app >> n
      case Const(name)     => app >> "~" >> name >> "~"
      case CodeUnit(c)     => app >> c.toInt >> "cu"
      case sv: SimpleValue => app >> sv.toString

  // TODO type
  // given typeRule: Rule[Type] = (app, ty) => ???
}