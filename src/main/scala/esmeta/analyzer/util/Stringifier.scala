package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{IRElem, Inst}
import esmeta.state.*
import esmeta.state.SimpleValue
import esmeta.ty.util.{Stringifier => TyStringifier}
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

  import TyStringifier.given

  /** elements */
  given elemRule: Rule[AnalyzerElem] = (app, elem) =>
    elem match
      case elem: View         => viewRule(app, elem)
      case elem: ControlPoint => cpRule(app, elem)
      case elem: AValue       => avRule(app, elem)
      case elem: TypeMismatch => mismatchRule(app, elem)
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
    app >> cp.func.name >> "[" >> cp.func.id >> "]:"
    app >> (cp match
      case NodePoint(_, node, view) => node.simpleString
      case ReturnPoint(func, view)  => "RETURN"
    )
    if (cp.view.isEmpty) app
    else app >> ":" >> cp.view

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
      case Nt(name, params) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "nt<" >> name
        if (!params.isEmpty) app >> "[" >> params >> "]"
        app >> ">"
      case Math(n)         => app >> n
      case Const(name)     => app >> "~" >> name >> "~"
      case CodeUnit(c)     => app >> c.toInt >> "cu"
      case sv: SimpleValue => app >> sv.toString

  // specification type mismatches
  given mismatchRule: Rule[TypeMismatch] = (app, m) =>
    given Rule[Inst] = addLoc
    m match
      case ParamTypeMismatch(callerNp, calleeRp, paramName) =>
        ???
      // app >> "[CallMismatch] " >> caller.name
      // loc.fold(app)(app >> " @ " >> _.toString)
      // app :> "- expected: " >> param.ty
      // app >> " (" >> param.lhs >> " @ " >> callee.name >> ")"
      // app :> "- actual  : " >> arg
      case ArityMismatch(callerNp, calleeRp, expected, actual) =>
        app >> "[ArityMismatch] " >> callerNp.func.name
        app >> callerNp.node.callInst
        val (from, to) = expected
        app :> "- expected: "
        if (from == to) app >> from
        else app >> "[" >> from >> ", " >> to >> "]"
        app >> " for " >> calleeRp.func.name
        app :> "- actual  : " >> actual

  private val addLoc: Rule[Inst] = (app, inst) => {
    for {
      lang <- inst.langOpt
      loc <- lang.loc
    } app >> " @ " >> loc.toString
    app
  }

  // TODO type
  // given typeRule: Rule[Type] = (app, ty) => ???
}
