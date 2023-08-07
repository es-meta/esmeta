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

// type check failure
case class TypeCheckFail(msg: Option[String])
  extends AnalysisError("type check failed." + msg.fold("")(LINE_SEP + _))

// invalid analysis point merge
case class InvalidAnalysisPointMerge(
  lpoint: AnalysisPoint,
  rpoint: AnalysisPoint,
) extends AnalysisError(
    s"invalid analysis point merge:$LINE_SEP" +
    s"```$lpoint``` and$LINE_SEP" +
    s"```$rpoint```",
  )

// invalid type error merge
case class InvalidTypeErrorMerge(
  lerror: TypeError,
  rerror: TypeError,
) extends AnalysisError(
    s"invalid type error merge:$LINE_SEP" +
    s"```$lerror``` and$LINE_SEP" +
    s"```$rerror```",
  )
