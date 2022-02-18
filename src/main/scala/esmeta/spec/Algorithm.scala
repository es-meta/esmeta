package esmeta.spec

import esmeta.ir.Func
import esmeta.lang.Step
import esmeta.spec.util.*
import org.jsoup.nodes.Element

// -----------------------------------------------------------------------------
// abstract algorithms
// -----------------------------------------------------------------------------
case class Algorithm(
  head: Head,
  elem: Element,
  body: Step,
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

/** syntax-directed operation (SDO) heads */
case class SyntaxDirectedOperationHead(
  target: Option[SyntaxDirectedOperationHead.Target],
  methodName: String,
  isStatic: Boolean,
  withParams: List[Param],
) extends Head
object SyntaxDirectedOperationHead:
  case class Target(
    lhsName: String,
    idx: Int,
    subIdx: Int,
    rhsParams: List[Param],
  ) extends SpecElem
type SdoHeadTarget = SyntaxDirectedOperationHead.Target

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
  kind: Param.Kind = Param.Kind.Normal,
  ty: String = "", // TODO more precisely represent parameter types
) extends SpecElem
object Param:
  enum Kind extends SpecElem:
    case Normal, Optional, Variadic, Ellipsis
