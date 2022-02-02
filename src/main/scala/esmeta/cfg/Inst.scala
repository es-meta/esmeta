package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.Locational

// CFG instructions
sealed trait Inst extends CFGElem with Locational
object Inst extends Parser.From[Inst]
case class IExpr(expr: Expr) extends Inst
case class ILet(lhs: Name, expr: Expr) extends Inst
case class IAssign(ref: Ref, expr: Expr) extends Inst
case class IDelete(ref: Ref) extends Inst
case class IPush(from: Expr, to: Expr, front: Boolean) extends Inst
case class IReturn(expr: Expr) extends Inst
case class IAssert(expr: Expr) extends Inst
case class IPrint(expr: Expr) extends Inst
