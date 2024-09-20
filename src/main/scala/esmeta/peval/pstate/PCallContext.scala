package esmeta.peval.pstate

import esmeta.ir.{Func as IRFunc, Local}

case class PCallContext(func: IRFunc, locals: PContext)
