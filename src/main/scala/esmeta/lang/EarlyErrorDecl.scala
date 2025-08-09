package esmeta.lang

sealed trait EarlyErrorDecl extends Syntax {

  /** check whether it is complete */
  def complete: Boolean = this match
    case _: YetSyntaxErrorDecl => false
    case _                     => true
}

// static semantic : early errors
case class YetSyntaxErrorDecl(yet: YetExpression) extends EarlyErrorDecl

case class ItIsASyntaxErrorDecl(cond: Condition, early: Boolean)
  extends EarlyErrorDecl

case class MustCoverDecl(covering: Literal, covered: Literal)
  extends EarlyErrorDecl
