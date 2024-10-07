package esmeta.error

import esmeta.peval.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ty.*

// TODO change to abstract class after refactoring of error in interp
sealed class PartialEvaluatorError(msg: String)
  extends ESMetaError(msg, "PartialEvaluatorError")

// // unexpected values
// class UnexpectedAValue(avalue: AValue, bound: AValue)
//   extends PartialEvaluatorError(
//     s"($avalue) <= ($bound) expected but it failed",
//   )
// case class NoAddrT(avalue: AValue)
//   extends UnexpectedAValue(avalue, AValue.AddrT)

class NotSupportedSyntax(ire: IRElem)
  extends PartialEvaluatorError(
    s"Partial-evaluating program having $ire is not yet supported",
  )

class ExpiredAllocator
  extends PartialEvaluatorError(
    s"Tried expired temporal variable allocator",
  )

class NonLiteral(v: Value)
  extends PartialEvaluatorError(
    s"Non-literal value $v is not printable",
  )

class UnexpectedKnown(s: StateElem, info: String = "")
  extends PartialEvaluatorError(
    s"$s is unexpected known value.${if (info.isEmpty()) then ""
    else s" for more information: $info"}",
  )

case class NoMoreInline()
  extends PartialEvaluatorError(
    "Faced too complicated case to inline; keep call inst. this error should be handled gracefully. if you encountered this as an unhandled failure, this is a bug.",
  )

class PEvalOptError(msg: String) extends PartialEvaluatorError(msg)
