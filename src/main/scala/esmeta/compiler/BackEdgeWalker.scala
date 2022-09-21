package esmeta.compiler

import esmeta.ir.{Inst, Expr, Ref}
import esmeta.ir.util.Walker

/** walker for adjusting backward edge from ir to lang */
case class BackEdgeWalker(
  fb: FuncBuilder,
  force: Boolean = false,
) extends Walker {
  override def walk(i: Inst) =
    if (force || i.langOpt.isEmpty) i.setLangOpt(fb.langs.headOption)
    super.walk(i)

  override def walk(e: Expr) =
    if (force || e.langOpt.isEmpty) e.setLangOpt(fb.langs.headOption)
    super.walk(e)

  override def walk(r: Ref) =
    if (force || r.langOpt.isEmpty) r.setLangOpt(fb.langs.headOption)
    super.walk(r)
}
