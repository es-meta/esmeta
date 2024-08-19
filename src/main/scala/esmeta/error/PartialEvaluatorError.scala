package esmeta.error

import esmeta.peval.*
import esmeta.peval.domain.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ty.*

// TODO change to abstract class after refactoring of error in interp
sealed class PartialEvaluatorError(msg: String)
  extends ESMetaError(msg, "PartialEvaluatorError")

// unexpected values
class UnexpectedAValue(avalue: AValue, bound: AValue)
  extends PartialEvaluatorError(
    s"($avalue) <= ($bound) expected but it failed",
  )
case class NoAddrT(avalue: AValue)
  extends UnexpectedAValue(avalue, AValue.AddrT)

class NotSupportedSyntax(ire: IRElem)
  extends PartialEvaluatorError(
    s"Partial-evaluating program having $ire is not yet supported",
  )


class ExpiredAllocator
  extends PartialEvaluatorError(
    s"Tried expired temporal variable allocator",
  )