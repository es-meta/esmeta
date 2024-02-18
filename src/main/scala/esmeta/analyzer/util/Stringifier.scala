package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{IRElem, LangEdge}
import esmeta.state.*
import esmeta.state.SimpleValue
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
        case elem: View          => viewRule(app, elem)
        case elem: AnalysisPoint => apRule(app, elem)
        case elem: AValue        => avRule(app, elem)
        case elem: TypeMismatch  => mismatchRule(app, elem)
    // TODO case ty: Type          => typeRule(app, ty)

    /** view */
    given viewRule: Rule[View] = (app, view) =>
      def auxRule[T](
        name: String,
      )(using Rule[T]): Rule[Iterable[T]] = (app, iter) =>
        if (iter.isEmpty) app
        else if (detail) iterableRule(s"[$name: ", ", ", "]")(app, iter)
        else app >> s"[$name: " >> iter.size >> "]"
      given cRule: Rule[Call] = (app, call) => app >> call.id
      given csRule: Rule[Iterable[Call]] = auxRule("call")
      given lRule: Rule[LoopCtxt] = {
        case (app, LoopCtxt(loop, depth)) => app >> s"${loop.id}($depth)"
      }
      given lsRule: Rule[Iterable[LoopCtxt]] = auxRule("loop")
      app >> view.calls >> view.loops

    // analysis points
    given apRule: Rule[AnalysisPoint] = (app, ap) =>
      given Rule[IRElem with LangEdge] = addLocRule
      ap match
        case cp: ControlPoint => cpRule(app, cp)
        case CallPoint(callerNp, calleeNp) =>
          app >> "function call from "
          app >> callerNp.func.name >> callerNp.node.callInst
          app >> " to " >> calleeNp.func.name
        case aap @ ArgAssignPoint(cp, idx) =>
          val param = aap.param
          app >> "argument assignment to "
          app >> (idx + 1).toOrdinal >> " parameter _" >> param.lhs.name >> "_"
          app >> " when " >> cp
        case InternalReturnPoint(irReturn, calleeRp) =>
          app >> "return statement in " >> calleeRp.func.name >> irReturn
        case ReturnIfAbruptPoint(riap, riaExpr) =>
          app >> "returnIfAbrupt"
          app >> "(" >> (if (riaExpr.check) "?" else "!") >> ") "
          app >> "in " >> riap.func.name >> riaExpr

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
        case Infinity(p)     => app >> (if (p) "+" else "-") >> "∞"
        case Const(name)     => app >> "~" >> name >> "~"
        case CodeUnit(c)     => app >> c.toInt >> "cu"
        case sv: SimpleValue => app >> sv.toString

    // specification type mismatches
    given mismatchRule: Rule[TypeMismatch] = (app, mismatch) =>
      mismatch match
        case ParamTypeMismatch(aap, actual) =>
          app >> "[ParamTypeMismatch] " >> aap
          app :> "- expected: " >> aap.param.ty
          app :> "- actual  : " >> actual
        case ReturnTypeMismatch(irp, actual) =>
          app >> "[ReturnTypeMismatch] " >> irp
          app :> "- expected: " >> irp.calleeRp.func.retTy
          app :> "- actual  : " >> actual
        case ArityMismatch(cp, actual) =>
          given Rule[(Int, Int)] = arityRangeRule
          app >> "[ArityMismatch] " >> cp
          app :> "- expected: " >> cp.func.arity
          app :> "- actual  : " >> actual
        case UncheckedAbruptCompletionMismatch(riap, actual) =>
          app >> "[UncheckedAbruptCompletionMismatch] " >> riap
          app :> "- actual  : " >> actual

    private val addLocRule: Rule[IRElem with LangEdge] = (app, elem) => {
      for {
        lang <- elem.langOpt
        loc <- lang.loc
      } app >> " " >> loc.toString
      app
    }

    private val arityRangeRule: Rule[(Int, Int)] = {
      case (app, (from, to)) =>
        if (from == to) app >> from
        else app >> "[" >> from >> ", " >> to >> "]"
    }

    // TODO type
    // given typeRule: Rule[Type] = (app, ty) => ???
  }
}
