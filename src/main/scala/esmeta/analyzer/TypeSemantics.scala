package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Return, Func => IRFunc, Name, Param, Local}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*

class TypeSemantics(
  npMap: Map[NodePoint[Node], AbsState],
) extends AbsSemantics(npMap) {

  /** type mismatches */
  def getMismatches: Set[TypeMismatch] =
    // get recorded parameter type mismatches
    val paramTypeMismatches = for {
      ((callerNp, calleeRp, param), argTy) <- argsForMismatch
    } yield ParamTypeMismatch(callerNp, calleeRp, param, argTy)
    // get recorded return type mismatches
    val returnTypeMismatches = for {
      ((elem, rp), retTy) <- retForMismatch
    } yield ReturnTypeMismatch(elem, rp, retTy)
    // return all type mismatches
    mismatches ++
    paramTypeMismatches ++
    returnTypeMismatches
  // record type mismatches
  private var mismatches: Set[TypeMismatch] = Set()
  private type CallEdgeWithParam = (NodePoint[Call], ReturnPoint, Param)
  private var argsForMismatch: Map[CallEdgeWithParam, ValueTy] = Map()
  private type RetEdge = (Return, ReturnPoint)
  private var retForMismatch: Map[RetEdge, ValueTy] = Map()

  /** handle calls */
  override def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
    method: Boolean = false,
  ): Unit =
    val NodePoint(callerFunc, call, view) = callerNp
    calleeFunc.retTy.ty match
      // Stop the propagation of analysis when it is unnecessary to analyze the
      // callee function because it has full type annotations.
      case retTy: ValueTy if calleeFunc.isParamTysDefined =>
        for {
          nextNode <- call.next
          nextNp = NodePoint(callerFunc, nextNode, View())
          retV = AbsValue(retTy)
          newSt = callerSt.defineLocal(call.lhs -> retV)
        } this += nextNp -> newSt
      // Otherwise, do original abstract call semantics
      case _ =>
        super.doCall(callerNp, callerSt, calleeFunc, args, captured, method)

  /** get local variables */
  override def getLocals(
    callerNp: NodePoint[Call],
    calleeRp: ReturnPoint,
    args: List[AbsValue],
    cont: Boolean,
    method: Boolean,
  ): Map[Local, AbsValue] = {
    // get parameters
    val params: List[Param] = calleeRp.func.irFunc.params
    // check arity
    val arity @ (from, to) = calleeRp.func.arity
    val len = args.length
    if (len < from || to < len)
      mismatches += ArityMismatch(callerNp, calleeRp, arity, len)
    // construct local type environment
    (for (((param, arg), idx) <- (params zip args).zipWithIndex) yield {
      val argTy = arg.ty
      val expected = param.ty.ty match
        case _: UnknownTy     => arg
        case paramTy: ValueTy =>
          // argument type check when parameter type is a known type
          if (!(argTy <= paramTy))
            val key = (callerNp, calleeRp, param)
            if (method && idx == 0) () /* ignore `this` for method-like calls */
            else
              argsForMismatch += key -> {
                argsForMismatch.get(key).fold(argTy)(_ || argTy)
              }
          AbsValue(paramTy)
      // force to set expected type for parameters
      param.lhs -> expected
    }).toMap
  }

  /** conversion to string */
  override def toString: String =
    (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString

  /** update return points */
  override def doReturn(elem: Return, rp: ReturnPoint, origRet: AbsRet): Unit =
    val ReturnPoint(func, view) = rp
    val newRet = if (func.isReturnComp) origRet.wrapCompletion else origRet
    val givenTy = newRet.value.ty
    val expected = rp.func.retTy.ty match
      case _: UnknownTy        => newRet
      case expectedTy: ValueTy =>
        // return type check when it is a known type
        if (!(givenTy <= expectedTy))
          val key = (elem, rp)
          retForMismatch += key -> {
            retForMismatch.get(key).fold(givenTy)(_ || givenTy)
          }
        AbsRet(AbsValue(expectedTy))
    super.doReturn(elem, rp, expected)

  /** conversion helper to string */
  given getRule: Rule[Iterable[Func]] = (app, funcs) =>
    import TyStringifier.given
    given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
    app >> "-" * 80
    for (func <- funcs) {
      val rp = ReturnPoint(func, View())
      app :> "   " >> func.headString
      val fname = func.name
      val entryNp = NodePoint(func, func.entry, View())
      val st = this(entryNp)
      val newParams =
        for (p <- func.params) yield p.lhs.name -> st.get(p.lhs, entryNp).ty
      app :> "-> " >> "def "
      app >> func.irFunc.kind.toString >> fname >> newParams
      app >> ": " >> rpMap.get(rp).fold(func.retTy.ty)(_.value.ty)
      app :> "-" * 80
    }
    app

  given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
    import TyStringifier.given
    val (param, ty) = pair
    app >> param
    if (ty.absent) app >> "?"
    app >> ": " >> ty -- AbsentT
}
