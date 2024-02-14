package esmeta.ty.util

import esmeta.state.{Nt, Number}
import esmeta.ty.*
import esmeta.util.*

/** a walker for types */
trait Walker extends BasicWalker {

  /** type elements */
  def walk(ty: TyElem): TyElem = ty match
    case ty: Ty          => walk(ty)
    case ty: CompTy      => walk(ty)
    case ty: PureValueTy => walk(ty)
    case ty: RecordTy    => walk(ty)
    case ty: ListTy      => walk(ty)
    case ty: SubMapTy    => walk(ty)
    case ty: MathTy      => walk(ty)
    case ty: InfinityTy  => walk(ty)
    case ty: BoolTy      => walk(ty)

  /** types */
  def walk(ty: Ty): Ty = ty match
    case ty: UnknownTy => walk(ty)
    case ty: ValueTy   => walk(ty)

  /** unknown types */
  def walk(ty: UnknownTy): UnknownTy = UnknownTy(
    walkOpt(ty.msg, walk),
  )

  /** value types */
  def walk(ty: ValueTy): ValueTy = ValueTy(
    walk(ty.comp),
    walk(ty.pureValue),
    walk(ty.subMap),
  )

  /** completion record types */
  def walk(ty: CompTy): CompTy = CompTy(
    walk(ty.normal),
    walkBSet(ty.abrupt, walk),
  )

  /** pure value types */
  def walk(ty: PureValueTy): PureValueTy =
    if (ty.isTop) ty
    else
      PureValueTy(
        walkClo(ty.clo),
        walkCont(ty.cont),
        walkName(ty.name),
        walk(ty.record),
        walk(ty.list),
        walk(ty.symbol),
        walkAst(ty.astValue),
        walkNt(ty.nt),
        walkCodeUnit(ty.codeUnit),
        walkConst(ty.const),
        walkMath(ty.math),
        walkInfinity(ty.infinity),
        walkNumber(ty.number),
        walkBigInt(ty.bigInt),
        walkStr(ty.str),
        walkBool(ty.bool),
        walkUndef(ty.undef),
        walkNull(ty.nullv),
        walkAbsent(ty.absent),
      )

  /** closure types */
  def walkClo(clo: BSet[String]): BSet[String] = walkBSet(clo, walk)

  /** continuation types */
  def walkCont(cont: BSet[Int]): BSet[Int] = walkBSet(cont, walk)

  /** AST value types */
  def walkAst(ast: AstValueTy): AstValueTy = ast match
    case AstTopTy         => AstTopTy
    case AstNameTy(names) => AstNameTy(walkSet(names, walk))
    case AstSingleTy(name, idx, subIdx) =>
      AstSingleTy(walk(name), walk(idx), walk(subIdx))

  /** nt types */
  def walkNt(nt: BSet[Nt]): BSet[Nt] =
    walkBSet(nt, walk)

  /** nt */
  def walk(nt: Nt): Nt = Nt(
    walk(nt.name),
    walkList(nt.params, walk),
  )

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Boolean = walk(codeUnit)

  /** constant types */
  def walkConst(const: BSet[String]): BSet[String] = walkBSet(const, walk)

  /** mathematical value types */
  def walkMath(math: MathTy): MathTy = math match
    case MathSetTy(set) => MathSetTy(walkSet(set, walk))
    case _              => math
  def walk(math: BigDecimal): BigDecimal = math

  /** infinity types */
  def walkInfinity(infinity: InfinityTy): InfinityTy =
    InfinityTy(infinity.pos.map(walk))

  /** number types */
  def walkNumber(number: NumberTy): NumberTy = number match
    case NumberSetTy(set) => NumberSetTy(walkSet(set, walk))
    case _                => number
  def walk(number: Number): Number = number

  /** big integer types */
  def walkBigInt(bigInt: Boolean): Boolean = walk(bigInt)

  /** string types */
  def walkStr(str: BSet[String]): BSet[String] = walkBSet(str, walk)

  /** boolean types */
  def walkBool(bool: BoolTy): BoolTy = BoolTy(walkSet(bool.set, walk))

  /** undefined types */
  def walkUndef(undef: Boolean): Boolean = walk(undef)

  /** null types */
  def walkNull(nullv: Boolean): Boolean = walk(nullv)

  /** absent types */
  def walkAbsent(absent: Boolean): Boolean = walk(absent)

  /** name types */
  def walkName(name: NameTy): NameTy = NameTy(walkBSet(name.set, walk))

  /** record types */
  def walk(ty: RecordTy): RecordTy =
    import RecordTy.*
    ty match
      case Top       => Top
      case Elem(map) => Elem(walkMap(map, walk, walk))

  /** list types */
  def walk(ty: ListTy): ListTy = ListTy(
    walkOpt(ty.elem, walk),
  )

  /** sub map types */
  def walk(ty: SubMapTy): SubMapTy = SubMapTy(
    walk(ty.key),
    walk(ty.value),
  )
}
