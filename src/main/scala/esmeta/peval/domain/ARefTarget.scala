package esmeta.peval.domain

import esmeta.ir.Var

enum ARefTarget:
  case AVar(x: Var)
  case AField(base: PValue, field: PValue)

  