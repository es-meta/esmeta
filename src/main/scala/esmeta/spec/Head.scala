package esmeta.spec

/** algorithm heads */
sealed trait Head extends SpecElem { val retTy: Type }

/** abstract operation (AO) heads */
case class AbstractOperationHead(
  isHostDefined: Boolean,
  name: String,
  params: List[Param],
  retTy: Type,
) extends Head

/** numeric method heads */
case class NumericMethodHead(
  ty: Type,
  name: String,
  params: List[Param],
  retTy: Type,
) extends Head

/** syntax-directed operation (SDO) heads */
case class SyntaxDirectedOperationHead(
  target: Option[SdoHeadTarget],
  methodName: String,
  isStatic: Boolean,
  withParams: List[Param],
  retTy: Type,
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
  retTy: Type,
) extends Head

/** internal method heads */
case class InternalMethodHead(
  methodName: String,
  receiverParam: Param,
  params: List[Param],
  retTy: Type,
) extends Head

/** buil-in heads */
case class BuiltinHead(
  ref: BuiltinHead.Ref,
  params: List[Param],
  retTy: Type,
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
