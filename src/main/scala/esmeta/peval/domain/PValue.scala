package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.peval.domain.PVTopTy.bigInt

/** Partial Value - Either Expr or Value */
sealed trait PartialValue
case class PValue(val ty: AValue, private val expr: Expr) extends PartialValue:
  // Use a type with a finer granularity than Expr for absExpr

  lazy val knownValue: Option[Value] =
    this.ty.getSingle match
      case Many      => None
      case One(elem) => Some(elem)
      case Zero      => None

  /** to print */
  lazy val asValidExpr: Expr = this.expr
