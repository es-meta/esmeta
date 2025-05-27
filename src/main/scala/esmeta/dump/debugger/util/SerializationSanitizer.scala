package esmeta.dump.debugger.util

import esmeta.*
import esmeta.dump.DUMMY_BODY
import esmeta.ir.{Type as IRType, *}
import esmeta.ir.util.Walker as IRWalker

/** removes informations that are lost in serialization */
object SerializationSanitizer extends IRWalker {
  override def walk(func: Func): Func =
    val Func(main, kind, name, params, retTy, body, algo) = func
    Func(
      main,
      kind,
      name,
      params.map(walk),
      walk(retTy),
      walk(body),
      algo,
    )

  override def walk(param: Param): Param = {
    val Param(lhs, ty, optional, _) = param
    Param(lhs, walk(ty), optional, None)
  }

  override def walk(inst: Inst): Inst = inst match
    case IIf(cond, thenInst, elseInst, isAbruptInst) =>
      IIf(
        walk(cond),
        walk(thenInst),
        walk(elseInst),
        false, // as default
      )
    case IWhile(cond, body) => IWhile(walk(cond), walk(body))
    case ISeq(insts)        => ISeq(insts.map(walk))
    case _                  => super.walk(inst)

  override def walk(ty: IRType): IRType = ty match
    case IRType(ty, _) => IRType(ty, None)

}
