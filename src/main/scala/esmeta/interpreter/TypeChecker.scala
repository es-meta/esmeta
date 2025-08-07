package esmeta.interpreter

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*

/** runtime type checker */
case class TypeChecker(st: State) {

  /** check valid field update check */
  def fieldUpdateCheck(ref: Ref, tgt: RefTarget, e: Expr, v: Value): Unit =
    (ref, getRecordField(tgt)) match // TODO ignore possible fields
      case (field: Field, Some(rec, f)) if !st.exists(tgt) =>
        val fp = FieldPoint(st.context.func, curNode, field)
        st.typeErrors += InvalidFieldError(fp, f, st.typeOf(rec))
      case _ =>

  /** check parameter types */
  def paramTyCheck(call: Call, f: Func, k: Int, x: Param, arg: Value): Unit =
    val paramTy = x.ty.ty.toValue
    // TODO remove this check in runtime type checker
    // ignore this type check for methods
    if (f.isMethod && k == 0) {} else if (
      paramTy.safeContains(arg, st) == Some(false)
    )
      val callPoint = CallPoint(st.context.func, call, f)
      val aap = ArgAssignPoint(callPoint, k)
      st.typeErrors += ParamTypeMismatch(aap, st.typeOf(arg))

  /** check return type */
  def returnTyCheck(ret: IReturn, retVal: Value): Unit =
    val retTy = st.context.func.irFunc.retTy.ty.toValue
    if (retTy.isDefined && retTy.safeContains(retVal, st) == Some(false))
      val irp = InternalReturnPoint(st.context.func, curNode, ret)
      st.typeErrors += ReturnTypeMismatch(irp, st.typeOf(retVal))

  private def curNode: Node = st.context.cursor match
    case NodeCursor(_, node, _) => node
    case _                      => raise("cursor is not node cursor")

  private def getRecordField(tgt: RefTarget): Option[(RecordObj, String)] =
    tgt match
      case FieldTarget(addr: Addr, Str(field)) =>
        st(addr) match
          case rec: RecordObj => Some(rec -> field)
          case _              => None
      case _ => None
}
