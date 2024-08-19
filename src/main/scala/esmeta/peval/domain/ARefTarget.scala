package esmeta.peval.domain

import esmeta.ir.Var

enum ARefTarget:
  case AVarTarget(x: Var)
  case AFieldTarget(base: PValue, field: PValue)
