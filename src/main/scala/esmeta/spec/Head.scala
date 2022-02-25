package esmeta.spec

/** algorithm heads */
sealed trait Head extends SpecElem

/** abstract operation (AO) heads */
case class AbstractOperationHead(
  name: String,
  params: List[Param],
  isHostDefined: Boolean,
) extends Head

/** numeric method heads */
case class NumericMethodHead(
  ty: Type,
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
  ref: BuiltinHead.Ref,
  params: List[Param],
) extends Head
object BuiltinHead:
  enum Ref extends SpecElem:
    case IntrinsicBase(name: String)
    case NormalBase(name: String)
    case NormalAccess(base: Ref, name: String)
    case Getter(base: Ref)
    case Setter(base: Ref)
    case SymbolAccess(base: Ref, symbol: String)
    case YetRef(name: String)
