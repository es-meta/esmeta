package esmeta.peval.util

import esmeta.cfg.{CFG}
import esmeta.ir.*
import esmeta.ir.util.{UnitWalker}
import esmeta.peval.pstate.*
import scala.collection.mutable.{Set as MSet}

// collects newly let/assigned locals
class LocalImpact private () extends UnitWalker {

  private val locals = MSet.empty[Local]

  def result: Set[Local] = Set.from(locals)

  override def walk(inst: Inst): Unit = inst match
    case IExpr(expr)                   =>
    case ILet(lhs, expr)               => locals += lhs
    case IAssign(ref, expr)            =>
    case IExpand(base, expr)           =>
    case IDelete(base, expr)           =>
    case IPush(elem, list, front)      =>
    case IPop(lhs, list, front)        => locals += lhs
    case IReturn(expr)                 =>
    case IAssert(expr)                 =>
    case IPrint(expr)                  =>
    case INop()                        =>
    case IIf(cond, thenInst, elseInst) => walk(thenInst); walk(elseInst)
    case IWhile(cond, body)            => walk(body)
    case ICall(lhs, fexpr, args)       => locals += lhs
    case ISdoCall(lhs, base, op, args) => locals += lhs
    case ISeq(insts)                   => insts.foreach(walk)
}

object LocalImpact {
  def apply(inst: Inst)(using renamer: Renamer, ctx: PContext, cfg: CFG) =
    val li = new LocalImpact()
    li.walk(inst)
    li.result.map(x => renamer.get(x, ctx))
}
