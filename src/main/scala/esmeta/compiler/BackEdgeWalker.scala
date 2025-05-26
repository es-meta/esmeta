package esmeta.compiler

import esmeta.ir.{Inst, Expr, Ref, IRElem}
import esmeta.ir.util.UnitWalker

/** walker for adjusting backward edge from ir to lang */
class BackEdgeWalker(fb: FuncBuilder) extends UnitWalker {
  private var overriden = false
  def apply[T <: IRElem](elem: T): T = apply(elem, false)
  def apply[T <: IRElem](elem: T, overriden: Boolean): T =
    this.overriden = overriden
    walk(elem)
    this.overriden = false
    elem
  override def walk(inst: Inst): Unit = if (overriden || inst.loc.isEmpty)
    inst.setLangOpt(fb.langs.headOption)
    super.walk(inst)

  override def walk(expr: Expr): Unit = if (overriden || expr.loc.isEmpty)
    expr.setLangOpt(fb.langs.headOption)
    super.walk(expr)

  override def walk(ref: Ref): Unit = if (overriden || ref.loc.isEmpty)
    ref.setLangOpt(fb.langs.headOption)
    super.walk(ref)
}
