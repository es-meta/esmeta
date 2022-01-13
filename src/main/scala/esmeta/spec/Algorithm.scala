package esmeta.spec

import esmeta.lang.Block
import Stringifier.*

// -----------------------------------------------------------------------------
// abstract algorithms
// -----------------------------------------------------------------------------
case class Algorithm(
  head: Head,
  id: String,
  body: Block,
  code: String,
) extends SpecElem

// -----------------------------------------------------------------------------
// algorithm heads
// -----------------------------------------------------------------------------
sealed trait Head extends SpecElem

/** abstract operation (AO) heads */
case class AbstractOperationHead(
  name: String,
  params: List[Param],
  isHostDefined: Boolean,
) extends Head

/** numeric method heads */
case class NumericMethodHead(
  ty: String,
  name: String,
  params: List[Param],
) extends Head

/** syntax-directed operation (SOD) heads */
case class SyntaxDirectedOperationHead(
  lhsName: String,
  idx: Int,
  subIdx: Int,
  rhsParams: List[Param],
  methodName: String,
  isStatic: Boolean,
  withParams: List[Param],
) extends Head

/** concrete method heads */
case class ConcreteMethodHead(
  methodName: String,
  receiverParam: Param,
  params: List[Param],
) extends Head

/** internal method heads */
case class InternalMethodHead(
  methodName: String,
  receiverParam: Param,
  params: List[Param],
) extends Head

/** buil-in heads */
case class BuiltinHead(
  ref: String, // TODO more precisely represent references
  params: List[Param],
) extends Head

// -----------------------------------------------------------------------------
// algorithm parameters
// -----------------------------------------------------------------------------
case class Param(
  name: String,
  kind: Param.Kind,
  ty: String, // TODO more precisely represent parameter types
) extends SpecElem
object Param:
  enum Kind extends SpecElem:
    case Normal, Optional, Variadic
