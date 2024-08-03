package esmeta.compiler

import esmeta.ir.*
import esmeta.ir.util.*

class ReturnModifier(fb: FuncBuilder) extends Walker {
  override def walk(inst: Inst): Inst = inst match
    case IReturn(expr) =>
      val x = if (isPure(expr)) expr else fb.newTIdWithExpr(expr)._2
      val (y, yExpr) = fb.newTIdWithExpr
      ISeq(
        List(
          IIf(
            isCompletion(x),
            IReturn(x),
            emptyInst,
          ),
          ICall(y, EClo("NormalCompletion", Nil), List(x)),
          IReturn(yExpr),
        ),
      )
    case _ => super.walk(inst)
}
