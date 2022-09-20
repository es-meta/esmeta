package esmeta.ty.util

import esmeta.state.{Nt, Number}
import esmeta.ty.*
import esmeta.util.*

/** a unit walker for types */
trait UnitWalker extends BasicUnitWalker {

  /** type elements */
  def walk(ty: TyElem): Unit = ty match
    case ty: Ty          => walk(ty)
    case ty: CompTy      => walk(ty)
    case ty: PureValueTy => walk(ty)
    case ty: RecordTy    => walk(ty)
    case ty: ListTy      => walk(ty)
    case ty: SubMapTy    => walk(ty)

  /** types */
  def walk(ty: Ty): Unit = ty match
    case ty: UnknownTy => walk(ty)
    case ty: ValueTy   => walk(ty)

  /** unknown types */
  def walk(ty: UnknownTy): Unit =
    walkOpt(ty.msg, walk)

  /** value types */
  def walk(ty: ValueTy): Unit =
    walk(ty.comp)
    walk(ty.pureValue)
    walk(ty.subMap)

  /** completion record types */
  def walk(ty: CompTy): Unit =
    walk(ty.normal)
    walk(ty.abrupt)

  /** pure value types */
  def walk(ty: PureValueTy): Unit =
    walkClo(ty.clo)
    walkCont(ty.cont)
    walkName(ty.name)
    walk(ty.record)
    walk(ty.list)
    walk(ty.symbol)
    walkAst(ty.astValue)
    walkNt(ty.nt)
    walkCodeUnit(ty.codeUnit)
    walkConst(ty.const)
    walkMath(ty.math)
    walkNumber(ty.number)
    walkBigInt(ty.bigInt)
    walkStr(ty.str)
    walkBool(ty.bool)
    walkUndef(ty.undef)
    walkNull(ty.nullv)
    walkAbsent(ty.absent)

  /** closure types */
  def walkClo(clo: BSet[String]): Unit = walkBSet(clo, walk)

  /** continuation types */
  def walkCont(cont: BSet[Int]): Unit = walkBSet(cont, walk)

  /** AST value types */
  def walkAst(ast: AstValueTy): Unit = ast match
    case AstTopTy                       =>
    case AstNameTy(names)               => walkSet(names, walk)
    case AstSingleTy(name, idx, subIdx) => walk(name); walk(idx); walk(subIdx)

  /** nt types */
  def walkNt(nt: BSet[Nt]): Unit =
    walkBSet(nt, walk)

  /** nt */
  def walk(nt: Nt): Unit =
    walk(nt.name)
    walkList(nt.params, walk)

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Unit = walk(codeUnit)

  /** constant types */
  def walkConst(const: Set[String]): Unit = walkSet(const, walk)

  /** mathematical value types */
  def walkMath(math: BSet[BigDecimal]): Unit = walkBSet(math, walk)
  def walk(math: BigDecimal): Unit = {}

  /** number types */
  def walkNumber(number: BSet[Number]): Unit = walkBSet(number, walk)
  def walk(number: Number): Unit = {}

  /** big integer types */
  def walkBigInt(bigInt: Boolean): Unit = walk(bigInt)

  /** string types */
  def walkStr(str: BSet[String]): Unit = walkBSet(str, walk)

  /** boolean types */
  def walkBool(bool: Set[Boolean]): Unit = walkSet(bool, walk)

  /** undefined types */
  def walkUndef(undef: Boolean): Unit = walk(undef)

  /** null types */
  def walkNull(nullv: Boolean): Unit = walk(nullv)

  /** absent types */
  def walkAbsent(absent: Boolean): Unit = walk(absent)

  /** name types */
  def walkName(name: NameTy): Unit = walkSet(name.set, walk)

  /** record types */
  def walk(ty: RecordTy): Unit =
    walkMap(ty.map, walk, walkOpt(_, walk))

  /** list types */
  def walk(ty: ListTy): Unit =
    walkOpt(ty.elem, walk)

  /** sub map types */
  def walk(ty: SubMapTy): Unit =
    walk(ty.key)
    walk(ty.value)
}
