package esmeta.ir.util

import esmeta.ir.AllocExpr

// allocation site setters
class AllocSiteSetter extends UnitWalker {
  private var asiteCount: Int = 0
  private def newAsite: Int = { val id = asiteCount; asiteCount += 1; id }
  override def walk(expr: AllocExpr): Unit =
    expr.asite = newAsite
    super.walk(expr)
}
