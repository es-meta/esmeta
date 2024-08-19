package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.peval.domain.AValueTop.bigInt
import esmeta.peval.domain.AValueExistenceTop.avalue

/** Partial Value - Abstract Value */

case class PValueExistence(val ty: AValueExistence, private val expr: Expr):

  /** to print */
  lazy val asValidExpr: Expr = this.expr

object PValueExistence:
  def uninit: PValueExistence = PValueExistence(
    AValueExistence(uninit = BoolTy(Set(true))),
    ???,
  )

  def from(pv: PValue) = PValueExistence(
    AValueExistence(avalue = pv.ty),
    pv.asValidForm,
  )
