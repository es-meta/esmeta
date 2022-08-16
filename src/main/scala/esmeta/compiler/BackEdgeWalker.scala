package esmeta.compiler

import esmeta.ir.Inst
import esmeta.ir.util.Walker

/** walker for adjusting backward edge from ir to lang */
case class BackEdgeWalker(
  fb: FuncBuilder,
  force: Boolean = false,
) extends Walker {
  override def walk(i: Inst) =
    if (force || i.langOpt.isEmpty) i.setLangOpt(fb.langs.headOption)
    super.walk(i)
}
