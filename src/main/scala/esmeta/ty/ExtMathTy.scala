package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.util.*
import esmeta.state.*
import esmeta.ty.util.Parser

/** extended math value types */
case class ExtMathTy(
  val math: BSet[Math] = Fin(),
  val inf: Flat[MathInf] = Zero,
) extends TyElem
  with Lattice[ExtMathTy] {
  import ExtMathTy.*

  /** bottom check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => ExtMathTy): Boolean = (this eq that) || (
    this.math <= that.math &&
    this.inf <= that.inf
  )

  /** union type */
  def ||(that: => ExtMathTy): ExtMathTy =
    if (this eq that) this
    else
      ExtMathTy(
        this.math || that.math,
        this.inf || that.inf,
      )

  /** intersection type */
  def &&(that: => ExtMathTy): ExtMathTy =
    if (this eq that) this
    else
      ExtMathTy(
        this.math && that.math,
        this.inf && that.inf,
      )

  /** prune type */
  def --(that: => ExtMathTy): ExtMathTy =
    if (this eq that) Bot
    else
      ExtMathTy(
        this.math -- that.math,
        this.inf -- that.inf,
      )

  /** get single value */
  def getSingle: Flat[ExtMath] = this.math.getSingle || this.inf.getSingle
}

object ExtMathTy extends Parser.From(Parser.extMathTy) {
  lazy val Top = ExtMathTy(Inf, Many)
  lazy val Bot = ExtMathTy()
}
