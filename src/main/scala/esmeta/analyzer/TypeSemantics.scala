package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, Name, Param, Local}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*

class TypeSemantics(
  npMap: Map[NodePoint[Node], AbsState],
) extends AbsSemantics(npMap) {

  /** type mismatches */
  var mismatches: Set[TypeMismatch] = Set()

  /** handle calls */
  override def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
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
        super.doCall(callerNp, callerSt, calleeFunc, args, captured)

  /** get local variables */
  override def getLocals(
    callerNp: NodePoint[Call],
    calleeRp: ReturnPoint,
    params: List[Param],
    args: List[AbsValue],
    cont: Boolean = false,
  ): Map[Local, AbsValue] = {
    val arity @ (from, to) = calleeRp.func.arity
    val len = args.length
    if (len < from || to < len)
      mismatches += ArityMismatch(callerNp, calleeRp, arity, len)
    (for {
      (param, arg) <- (params zip args)
    } yield param.lhs -> arg).toMap
  }

  /** conversion to string */
  override def toString: String =
    (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString

  /** update return points */
  override def doReturn(rp: ReturnPoint, origRet: AbsRet): Unit =
    val ReturnPoint(func, view) = rp
    val newRet = rp.func.retTy.ty match
      case _: UnknownTy => origRet
      case ty: ValueTy  => AbsRet(AbsValue(ty))
    super.doReturn(rp, newRet)

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
