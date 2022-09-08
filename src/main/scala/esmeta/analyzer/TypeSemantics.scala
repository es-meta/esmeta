package esmeta.analyzer

import esmeta.ir.Name
import esmeta.cfg.*
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*

class TypeSemantics(
  npMap: Map[NodePoint[Node], AbsState],
) extends AbsSemantics(npMap) {

  /** handle calls */
  override def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
  ): Unit =
    // get parameter types
    val newArgs = (args zip calleeFunc.params).map {
      case (arg, param) =>
        param.ty.ty match
          case _: UnknownTy => arg
          case ty: ValueTy  => AbsValue(ty)
    }
    val NodePoint(callerFunc, call, view) = callerNp
    calleeFunc.retTy.ty match
      case retTy: ValueTy if calleeFunc.isParamTysDefined =>
        for {
          nextNode <- call.next
          nextNp = NodePoint(callerFunc, nextNode, View())
          retV = AbsValue(retTy)
          newSt = callerSt.defineLocal(call.lhs -> retV)
        } this += nextNp -> newSt
      case _ =>
        super.doCall(callerNp, callerSt, calleeFunc, newArgs, captured)

  /** conversion to string */
  override def toString: String =
    (new Appender >> analyzedFuncs.toList.sortBy(_.name)).toString

  /** conversion helper to string */
  given getRule: Rule[Iterable[Func]] = (app, funcs) =>
    import TyStringifier.given
    given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
    app >> "-" * 80
    for (func <- funcs) {
      val rp = ReturnPoint(func, View())
      rpMap.get(rp) match
        case None =>
          app :> "analysis of " >> func.name >> " does not terminate."
        case Some(ret) =>
          app :> "   " >> func.headString
          val fname = func.name
          val entryNp = NodePoint(func, func.entry, View())
          val st = this(entryNp)
          val newParams = for {
            p <- func.params
          } yield {
            p.lhs.name -> st.get(p.lhs, entryNp).ty
          }
          app :> "-> " >> "def "
          app >> func.irFunc.kind.toString >> fname >> newParams
          app >> ": " >> ret.value.ty
      app :> "-" * 80
    }
    app

  given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
    import TyStringifier.given
    val (param, ty) = pair
    app >> param
    if (ty.absent) app >> "?"
    app >> ": " >> ty -- AbsentT

  /** update return points */
  override def doReturn(rp: ReturnPoint, origRet: AbsRet): Unit =
    val ReturnPoint(func, view) = rp
    val newRet = rp.func.retTy.ty match
      case _: UnknownTy => origRet
      case ty: ValueTy  => AbsRet(AbsValue(ty))
    super.doReturn(rp, newRet)
}
