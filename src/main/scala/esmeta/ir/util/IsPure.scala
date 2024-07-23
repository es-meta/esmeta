package esmeta.ir.util

import esmeta.ir.*

private val notSure = false

def isPure(r: Ref): Boolean = r match
  case Field(ref, expr) => notSure
  case Global(name)     => true
  case Name(name)       => true
  case Temp(idx)        => true
