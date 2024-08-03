package esmeta.ir.util

import esmeta.ir.*

private val notSure = false

// TODO Check if the expression does not have any side effects
def isPure(e: Expr): Boolean = e match
  case ERef(r)        => isPure(r)
  case _: LiteralExpr => true
  case _              => false

// TODO Check if the expression does not have any side effects
def isPure(r: Ref): Boolean = r match
  case Field(ref, expr) => notSure
  case Global(name)     => true
  case Name(name)       => true
  case Temp(idx)        => true
