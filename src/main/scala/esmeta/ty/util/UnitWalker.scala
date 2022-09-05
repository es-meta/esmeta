package esmeta.ty.util

import esmeta.state.Grammar
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
    walkNames(ty.names)
    walk(ty.record)
    walk(ty.list)
    walk(ty.symbol)
    walkAst(ty.astValue)
    walkGrammar(ty.grammar)
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
    case AstTopTy               =>
    case AstNameTy(names)       => walkSet(names, walk)
    case AstSingleTy(name, idx) => walk(name); walk(idx)

  /** grammar types */
  def walkGrammar(grammar: BSet[Grammar]): Unit =
    walkBSet(grammar, walk)

  /** grammar */
  def walk(grammar: Grammar): Unit =
    walk(grammar.name)
    walkList(grammar.params, walk)

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Unit = walk(codeUnit)

  /** constant types */
  def walkConst(const: Set[String]): Unit = walkSet(const, walk)

  /** mathematical value types */
  def walkMath(math: Boolean): Unit = walk(math)

  /** number types */
  def walkNumber(number: Boolean): Unit = walk(number)

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
  def walkNames(names: Set[String]): Unit = walkSet(names, walk)

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
