package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{IRElem, LangEdge}
import esmeta.state.*
import esmeta.ty.*
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
      case elem: View          => viewRule(app, elem)
      case elem: AnalysisPoint => apRule(app, elem)
      case elem: AValue        => avRule(app, elem)
      case elem: TypeError     => errorRule(app, elem)

  /** view */
  given viewRule: Rule[View] = (app, view) =>
    given Rule[Call] = (app, call) => app >> call.id.toString
    given Rule[LoopCtxt] = (app, ctxt) =>
      app >> ctxt.loop.id >> "(" >> ctxt.depth >> ")"
    def aux[T](name: String)(using f: Rule[T]): Rule[List[T]] = (app, xs) =>
      if (xs.isEmpty) app
      else
        app >> "[" >> name >> ": "
        if (detail) iterableRule(sep = ", ")(app, xs)
        else app >> xs.length
        app >> "]"

    val View(calls, loops, _, tys) = view
    aux[Call]("call")(app, calls)
    aux[LoopCtxt]("loop")(app, loops)
    aux[ValueTy]("ty")(app, tys)

  // analysis points
  given apRule: Rule[AnalysisPoint] = (app, ap) =>
    given Rule[IRElem with LangEdge] = addLocRule
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
      case InternalReturnPoint(calleeRp, irReturn) =>
        app >> "return statement in " >> calleeRp.func.name >> irReturn
      case ReturnIfAbruptPoint(cp, riaExpr) =>
        app >> "ReturnIfAbrupt"
        app >> "(" >> (if (riaExpr.check) "?" else "!") >> ") "
        app >> "in " >> cp.func.name >> riaExpr
      case PropBasePoint(propPoint) =>
        app >> "base in" >> propPoint
      case PropPoint(cp, prop) =>
        app >> "property lookup in " >> cp.func.name >> prop
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

  // values for analysis
  given avRule: Rule[AValue] = (app, av) =>
    av match
      case AComp(Const("normal"), v, _) =>
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

  // type errors in specifications
  given errorRule: Rule[TypeError] = (app, error) =>
    app >> "[" >> error.getClass.getSimpleName >> "] " >> error.point
    error match
      case ParamTypeMismatch(point, argTy) =>
        app :> "- expected: " >> point.param.ty
        app :> "- actual  : " >> argTy
      case ReturnTypeMismatch(point, retTy) =>
        app :> "- expected: " >> point.calleeRp.func.retTy
        app :> "- actual  : " >> retTy
      case UncheckedAbruptComp(point, ty) =>
        app :> "- actual  : " >> ty
      case InvalidPropBase(point, baseTy) =>
        app :> "- base    : " >> baseTy
      case UnaryOpTypeMismatch(point, operandTy) =>
        app :> "- operand : " >> operandTy
      case BinaryOpTypeMismatch(point, lhsTy, rhsTy) =>
        app :> "- lhs     : " >> lhsTy
        app :> "- rhs     : " >> rhsTy

  private val addLocRule: Rule[IRElem with LangEdge] = (app, elem) =>
    for {
      lang <- elem.langOpt
      loc <- lang.loc
    } app >> " " >> loc.toString
    app

  private val arityRangeRule: Rule[(Int, Int)] = {
    case (app, (from, to)) =>
      if (from == to) app >> from
      else app >> "[" >> from >> ", " >> to >> "]"
  }
}
