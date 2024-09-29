package esmeta.error

import esmeta.LINE_SEP
import esmeta.ir.Param

sealed abstract class AnalysisError(msg: String)
  extends ESMetaError(msg, s"AnalysisError")

// not supported
case class NotSupportedOperation(obj: Any, method: String)
  extends AnalysisError(s"${obj.getClass.getName}.$method is not supported")

// imprecise
case class AnalysisImprecise(msg: String) extends AnalysisError(msg)

// type check failure
case class TypeCheckFail(msg: Option[String])
  extends AnalysisError("type check failed." + msg.fold("")(LINE_SEP + _))
