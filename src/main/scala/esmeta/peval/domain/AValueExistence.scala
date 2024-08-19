package esmeta.peval.domain

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
import esmeta.error.NotSupported.{*, given}
import esmeta.state.*
import esmeta.ty.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** partial value types */
sealed trait AValueExistence extends Lattice[AValueExistence] {
  import AValueExistence.*

  def avalue: AValue
  def uninit: BoolTy

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean =
    if (this eq Bot) true
    else if (this eq Top) false
    else
      (
        this.avalue.isBottom &&
        this.uninit.isBottom
      )

  /** partial order/subset operator */
  def <=(that: => AValueExistence): Boolean =
    if ((this eq that) || (this eq Bot) || (that eq Top)) true
    else if (this eq Top) false
    else
      this.avalue <= that.avalue &&
      // XXX is this right?
      this.uninit <= that.uninit

  /** union type */
  def ||(that: => AValueExistence): AValueExistence =
    if (this eq that) this
    else if (this eq Bot) that
    else if (this eq Top) Top
    else if (that eq Bot) this
    else if (that eq Top) Top
    else
      AValueExistence(
        this.avalue || this.avalue,
        this.uninit || that.uninit,
      ).norm

  /** intersection type */
  def &&(that: => AValueExistence): AValueExistence =
    if (this eq that) this
    else if (this eq Bot) Bot
    else if (this eq Top) that
    else if (that eq Bot) Bot
    else if (that eq Top) this
    else
      AValueExistence(
        this.avalue && that.avalue,
        this.uninit && that.uninit,
      )

  /** prune type */
  def --(that: => AValueExistence): AValueExistence =
    if (this eq that) Bot
    else if (this eq Bot) Bot
    else if (that eq Bot) this
    else if (that eq Top) Bot
    else
      AValueExistence(
        this.avalue -- that.avalue,
        this.uninit -- that.uninit,
      )

  /** copy value type */
  def copied(
    avalue: AValue = avalue,
    uninit: BoolTy = uninit,
  ): AValueExistence = AValueExistence(
    avalue,
    uninit,
  )

  /** completion check */
  def isCompletion: Boolean = ??? // this <= CompT

  /** normalization */
  def norm: AValueExistence = if (
    avalue.isTop &&
    uninit.isTop
  ) AValueExistence.Top
  else this

  /** get single value */
  def getSingle: Flat[Value] =
    (avalue.getSingle.map((a) => a: Value)) ||
    (uninit.getSingle.map(Bool(_): Value))

  /** types having no field */
  def noField: AValueExistence = this match
    case AValueExistenceTop =>
      AValueExistenceElem(
        avalue = AValue.Top,
        uninit = BoolTy.Top,
      )
    case elem: AValueExistenceElem =>
      elem.copy(
        avalue = AValue.Bot,
        uninit = BoolTy.Bot,
      )
}

case object AValueExistenceTop extends AValueExistence {
  def avalue: AValue = AValue.Top
  def uninit: BoolTy = BoolTy.Top
}

case class AValueExistenceElem(
  avalue: AValue = AValue.Bot,
  uninit: BoolTy = BoolTy.Bot,
) extends AValueExistence
object AValueExistence { // extends Parser.From(Parser.AValueExistence)
  def apply(
    avalue: AValue = AValue.Bot,
    uninit: BoolTy = BoolTy.Bot,
  ): AValueExistence = AValueExistenceElem(
    avalue,
    uninit,
  ).norm
  lazy val Top: AValueExistence = AValueExistenceTop
  lazy val Bot: AValueExistence = AValueExistenceElem()
}
