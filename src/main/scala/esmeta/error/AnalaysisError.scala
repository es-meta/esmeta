package esmeta.error

import esmeta.LINE_SEP
import esmeta.ir.Param
import esmeta.analyzer.*

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

// type mismatches
case class TypeMismatchError(mismatches: Set[TypeMismatch])
  extends AnalysisError(
    s"${mismatches.size} type mismatches are detected" + LINE_SEP +
    mismatches.toList.map(_.toString).sorted.mkString(LINE_SEP),
  )
case class UnneceesaryIgnore(ignoreSet: Set[String])
  extends AnalysisError(
    s"${ignoreSet.size} names are not unused to ignore type mismatches:" +
    LINE_SEP + ignoreSet.toList.sorted.map("- " + _).mkString(LINE_SEP),
  )
