package esmeta.peval.util

import esmeta.ir.*

class NoLiterals private () {
  def prune(inst: Inst): Inst = inst match
    case ILet(lhs, expr)     => if (expr.isLiteral) then ISeq(Nil) else inst
    case IAssign(ref, expr)  => if (expr.isLiteral) then ISeq(Nil) else inst
    case IExpand(base, expr) => inst
    case IDelete(base, expr) => inst
    case IPush(elem, list, front)      => inst
    case IPop(lhs, list, front)        => inst
    case ICall(lhs, fexpr, args)       => inst
    case ISdoCall(lhs, base, op, args) => inst
    case ISeq(insts)                   => ISeq(insts.map(prune)).passCmt(inst)
    // no recursion for now
    case IIf(cond, thenInst, elseInst) =>
      inst // IIf(cond, prune(thenInst), prune(elseInst)).passCmt(inst) else inst
    case IWhile(cond, body) => inst // IWhile(cond, prune(body)).passCmt(inst)
    case _                  => inst

}

object NoLiterals {
  def apply(inst: Inst): Inst = new NoLiterals().prune(inst)
}
