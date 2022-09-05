package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.util.*
import esmeta.state.*
import esmeta.ty.util.Parser

/** AST value types */
sealed trait AstValueTy extends TyElem with Lattice[AstValueTy] {

  /** bottom check */
  def isBottom: Boolean = this == AstNameTy()

  /** partial order/subset operator */
  def <=(that: => AstValueTy): Boolean = (this, that) match
    case (AstBotTy, _) | (_, AstTopTy)    => true
    case (l: AstSingleTy, r: AstSingleTy) => l == r
    case (l: AstNonTopTy, r: AstNonTopTy) =>
      l.toName.names subsetOf r.toName.names
    case _ => false

  /** union type */
  def |(that: => AstValueTy): AstValueTy = (this, that) match
    case (AstTopTy, _) | (_, AstTopTy)              => AstTopTy
    case (AstBotTy, _)                              => that
    case (_, AstBotTy)                              => this
    case (l: AstSingleTy, r: AstSingleTy) if l == r => l
    case (l: AstNonTopTy, r: AstNonTopTy) =>
      AstNameTy(l.toName.names | r.toName.names)

  /** intersection type */
  def &(that: => AstValueTy): AstValueTy = (this, that) match
    case _ if this <= that                  => this
    case _ if that <= this                  => that
    case (AstNameTy(lset), AstNameTy(rset)) => AstNameTy(lset & rset)
    case _                                  => AstBotTy

  /** prune type */
  def --(that: => AstValueTy): AstValueTy = (this, that) match
    case _ if this <= that                  => AstBotTy
    case (AstNameTy(lset), AstNameTy(rset)) => AstNameTy(lset -- rset)
    case _                                  => this

  /** get single value */
  def getSingle: Flat[AValue] = Many
}
object AstValueTy extends Parser.From(Parser.astValueTy)
case object AstTopTy extends AstValueTy
sealed trait AstNonTopTy extends AstValueTy {
  def toName: AstNameTy = this match
    case ty: AstNameTy           => ty
    case AstSingleTy(name, _, _) => AstNameTy(Set(name))
}
case class AstNameTy(names: Set[String] = Set()) extends AstNonTopTy
case class AstSingleTy(name: String, idx: Int, subIdx: Int) extends AstNonTopTy
val AstBotTy = AstNameTy()
