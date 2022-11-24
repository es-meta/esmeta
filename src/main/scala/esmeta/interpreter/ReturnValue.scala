package esmeta.interpreter

import esmeta.ir.Return
import esmeta.state.Value

/** special class for handle return */
case class ReturnValue(value: Value, ret: Return) extends Throwable
