package esmeta.error

import esmeta.ir.Param
import esmeta.analyzer.domain.*

sealed abstract class AnalysisError(msg: String)
  extends ESMetaError(msg, s"AnalysisError")

// not supported
case class NotSupportedOperation(obj: Any, method: String)
  extends AnalysisError(s"${obj.getClass.getName}.$method is not supported")

// imprecise
case class AnalysisImprecise(msg: String) extends AnalysisError(msg)

// arity mismatches
case class AnalysisRemainingParams(ps: List[Param])
  extends AnalysisError(s"remaining parameters: ${ps.mkString(", ")}")
case class AnalysisRemainingArgs(as: List[AbsValue])
  extends AnalysisError(s"remaining arguments: ${as.mkString(", ")}")
