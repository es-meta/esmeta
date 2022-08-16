package esmeta.interpreter

import esmeta.state.Value

/** special class for handle return */
case class ReturnValue(value: Value) extends Throwable
