package esmeta.compiler

import esmeta.ir.{Inst, Expr, Ref, IRElem}
import esmeta.ir.util.UnitWalker

/** walker for adjusting backward edge from ir to lang */
class BackEdgeWalker(fb: FuncBuilder) extends UnitWalker {
  def apply[T <: IRElem](elem: T): T = { walk(elem); elem }
  override def walk(inst: Inst): Unit = if (inst.langOpt.isEmpty)
    inst.setLangOpt(fb.langs.headOption)
    super.walk(inst)

  override def walk(expr: Expr): Unit = if (expr.langOpt.isEmpty)
    expr.setLangOpt(fb.langs.headOption)
    super.walk(expr)

  override def walk(ref: Ref): Unit = if (ref.langOpt.isEmpty)
    ref.setLangOpt(fb.langs.headOption)
    super.walk(ref)
}
