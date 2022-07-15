package esmeta.error

import esmeta.ir.Func
import esmeta.analyzer.domain.*

sealed abstract class AnalysisError(msg: String)
  extends ESMetaError(s"[Analysis Error] $msg")

// imprecise
case class AnalysisImprecise(msg: String) extends AnalysisError(msg)

// arity mismatches
case class AnalysisRemainingParams(ps: List[Func.Param])
  extends AnalysisError(s"remaining parameters: ${ps.mkString(", ")}")
case class AnalysisRemainingArgs(as: List[AbsValue])
  extends AnalysisError(s"remaining arguments: ${as.mkString(", ")}")
