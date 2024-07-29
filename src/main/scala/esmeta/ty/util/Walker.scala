package esmeta.ty.util

import esmeta.state.{GrammarSymbol, Number, Math}
import esmeta.ty.*
import esmeta.util.*

/** a walker for types */
trait Walker extends BasicWalker {

  /** type elements */
  def walk(ty: TyElem): TyElem = ty match
    case elem: TyModel     => walk(elem)
    case elem: TyDecl      => walk(elem)
    case elem: Ty          => walk(elem)
    case elem: CompTy      => walk(elem)
    case elem: PureValueTy => walk(elem)
    case elem: RecordTy    => walk(elem)
    case elem: ListTy      => walk(elem)
    case elem: AstTy       => walk(elem)
    case elem: MapTy       => walk(elem)
    case elem: MathTy      => walk(elem)
    case elem: InfinityTy  => walk(elem)
    case elem: NumberTy    => walk(elem)
    case elem: BoolTy      => walk(elem)

  /** type models */
  def walk(ty: TyModel): TyModel = TyModel(walkMap(ty.decls, walk, walk))

  /** type declarations */
  def walk(ty: TyDecl): TyDecl = TyDecl(
    walk(ty.name),
    walkOpt(ty.parent, walk),
    walkMap(ty.rawFields, walk, walk),
  )

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
    walk(ty.map),
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
        walk(ty.record),
        walk(ty.list),
        walkAst(ty.ast),
        walkGrammarSymbol(ty.grammarSymbol),
        walkCodeUnit(ty.codeUnit),
        walkEnum(ty.enumv),
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
  def walkAst(ast: AstTy): AstTy =
    import AstTy.*
    ast match
      case Top               => Top
      case Simple(names)     => Simple(walkSet(names, walk))
      case Detail(name, idx) => Detail(walk(name), walk(idx))

  /** grammar symbol types */
  def walkGrammarSymbol(
    grammarSymbol: BSet[GrammarSymbol],
  ): BSet[GrammarSymbol] =
    walkBSet(grammarSymbol, walk)

  /** grammar symbols */
  def walk(grammarSymbol: GrammarSymbol): GrammarSymbol = GrammarSymbol(
    walk(grammarSymbol.name),
    walkList(grammarSymbol.params, walk),
  )

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Boolean = walk(codeUnit)

  /** enum types */
  def walkEnum(enumv: BSet[String]): BSet[String] = walkBSet(enumv, walk)

  /** mathematical value types */
  def walkMath(math: MathTy): MathTy = math match
    case MathSetTy(set) => MathSetTy(walkSet(set, walk))
    case _              => math
  def walk(math: Math): Math = math

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

  /** record types */
  def walk(ty: RecordTy): RecordTy =
    import RecordTy.*
    ty match
      case Detail(name, map) => Detail(name, walkMap(map, walk, walk))
      case Simple(set)       => Simple(walkSet(set, walk))

  /** list types */
  def walk(ty: ListTy): ListTy = ListTy(
    walkOpt(ty.elem, walk),
  )

  /** map types */
  def walk(ty: MapTy): MapTy = MapTy(
    walk(ty.key),
    walk(ty.value),
  )
}
