package esmeta.spec

/** algorithm heads */
sealed trait Head extends SpecElem {
  val retTy: Type

  /** get original parameters */
  def originalParams: List[Param] = this match
    case abs: AbstractOperationHead       => abs.params
    case numeric: NumericMethodHead       => numeric.params
    case sdo: SyntaxDirectedOperationHead => sdo.withParams
    case con: ConcreteMethodHead          => con.params
    case int: InternalMethodHead          => int.params
    case builtin: BuiltinHead             => builtin.params

  /** get function parameters */
  def funcParams: List[Param] = this match
    case head: AbstractOperationHead       => head.params
    case head: NumericMethodHead           => head.params
    case head: SyntaxDirectedOperationHead => Param("this") :: head.withParams
    case head: ConcreteMethodHead          => head.receiverParam :: head.params
    case head: InternalMethodHead          => head.receiverParam :: head.params
    case head: BuiltinHead =>
      List(Param("this"), Param("argumentsList"), Param("NewTarget"))

  /** get function name */
  def fname: String = this match
    case head: AbstractOperationHead =>
      head.name
    case head: NumericMethodHead =>
      s"${head.ty.normalizedName}::${head.name}"
    case head: SyntaxDirectedOperationHead =>
      val Target = SyntaxDirectedOperationHead.Target
      val pre = head.target.fold("<DEFAULT>") {
        case Target(lhsName, idx, subIdx, _) => s"$lhsName[$idx,$subIdx]"
      }
      s"$pre.${head.methodName}"
    case head: ConcreteMethodHead =>
      s"${head.receiverParam.ty.normalizedName}.${head.methodName}"
    case head: InternalMethodHead =>
      s"${head.receiverParam.ty.normalizedName}.${head.methodName}"
    case head: BuiltinHead =>
      s"INTRINSICS.${head.ref.normalized}"

}

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
  extension (ref: Ref) {
    def normalized: String = ref.toString.replace("%", "")
  }
