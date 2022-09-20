package esmeta.ty

import esmeta.ty.util.Parser
import esmeta.util.*

/** named record types */
case class NameTy(set: Set[String] = Set())
  extends TyElem
  with Lattice[NameTy] {
  import NameTy.*
  import TyModel.es.isSubTy

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) || set.isEmpty

  /** partial order/subset operator */
  def <=(that: => NameTy): Boolean =
    (this eq that) || isSubTy(this.set, that.set)

  /** union type */
  def ||(that: => NameTy): NameTy =
    if (this eq that) this
    else if (this.isBottom) that
    else if (that.isBottom) this
    else NameTy(this.set || that.set).norm

  /** intersection type */
  def &&(that: => NameTy): NameTy =
    if (this eq that) this
    else
      NameTy(
        this.set.filter(isSubTy(_, that.set)) ||
        that.set.filter(isSubTy(_, this.set)),
      )

  /** prune type */
  def --(that: => NameTy): NameTy =
    if (that.isBottom) this
    else NameTy(this.set.filter(!isSubTy(_, that.set)))

  // normalization
  def norm: NameTy =
    val res = NameTy(this.set.filter(x => {
      !isSubTy(x, this.set - x)
    }))
    res

  /** get single value */
  def getSingle: Flat[Nothing] = if (set.isEmpty) Zero else Many
}
object NameTy extends Parser.From(Parser.nameTy) {
  val Bot: NameTy = NameTy()
}
