package esmeta.peval.domain

import esmeta.ir.{BOp, EBinary, EBool, Expr}

case class PathCondition(exprs: List[Expr]):
  def guard: Expr =
    def aux(es: Iterable[Expr]): Expr = es match
      case Nil          => EBool(true)
      case head :: Nil  => head
      case head :: next => EBinary(BOp.And, head, aux(next))
    aux(this.exprs)

  def and(expr: Expr): PathCondition = new PathCondition(expr :: exprs)

object PathCondition:
  def apply(): PathCondition = PathCondition(Nil)
