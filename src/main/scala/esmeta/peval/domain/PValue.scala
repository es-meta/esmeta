package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.peval.domain.AValueTop.bigInt

/** Partial Value - Abstract Value */

case class PValue(val ty: AValue, private val expr: Expr)
  extends PartialElem[Value, Expr]:
  // Use a type with a finer granularity than Expr for absExpr

  lazy val known: Option[Value] = this.ty.getSingle match
    case One(elem) => Some(elem)
    case _         => None

  lazy val asValidForm: Expr = this.expr

  def addr: BSet[Addr] = this.ty.addr

/** to print */
