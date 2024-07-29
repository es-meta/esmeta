package esmeta.ty

import esmeta.util.*
import esmeta.state.*
import esmeta.ty.util.Parser

/** AST value types */
enum AstTy extends TyElem with Lattice[AstTy] {

  /** the top element */
  case Top

  /** a simple ast type with a set of nonterminal names */
  case Simple(set: Set[String])

  /** a detailed ast type with its type name and right-hand side index */
  case Detail(name: String, idx: Int)

  import AstTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => AstTy): Boolean = (this eq that) || (
    (this, that) match
      case (_, Top)                           => true
      case (Top, _)                           => false
      case (Detail(l, lidx), Detail(r, ridx)) => l == r && lidx == ridx
      case (Simple(ls), Detail(_, _))         => ls.isEmpty
      case (Simple(ls), Simple(rs))           => ls subsetOf rs
      case (Detail(l, lidx), Simple(rs))      => rs contains l
      case _                                  => false
  )

  /** union type */
  def ||(that: => AstTy): AstTy =
    if (this eq that) this
    else if (this <= that) that
    else if (that <= this) this
    else
      (this.names, that.names) match
        case (Inf, _) | (_, Inf) => Top
        case (Fin(ls), Fin(rs))  => Simple(ls ++ rs)

  /** intersection type */
  def &&(that: => AstTy): AstTy =
    if (this eq that) this
    else if (this <= that) this
    else if (that <= this) that
    else
      (this.names, that.names) match
        case (Inf, _)           => that
        case (_, Inf)           => this
        case (Fin(ls), Fin(rs)) => Simple(ls intersect rs)

  /** prune type */
  def --(that: => AstTy): AstTy =
    if (that.isBottom) this
    else
      (this, that) match
        case _ if this <= that            => Bot
        case (Simple(lset), Simple(rset)) => Simple(lset -- rset)
        case _                            => this

  /** get the set of type names */
  def names: BSet[String] = this match
    case Top             => Inf
    case Simple(names)   => Fin(names)
    case Detail(name, _) => Fin(Set(name))
}
object AstTy extends Parser.From(Parser.astTy) {
  lazy val Bot: Simple = Simple(Set())
}
