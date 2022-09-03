package esmeta.ty.util

import esmeta.state.Grammar
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
    walk(ty.abrupt),
  )

  /** pure value types */
  def walk(ty: PureValueTy): PureValueTy = PureValueTy(
    walkClo(ty.clo),
    walkCont(ty.cont),
    walkNames(ty.names),
    walk(ty.record),
    walk(ty.list),
    walk(ty.symbol),
    walkAst(ty.astValue),
    walkGrammar(ty.grammar),
    walkCodeUnit(ty.codeUnit),
    walkConst(ty.const),
    walkMath(ty.math),
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
  def walkCont(cont: BSet[String]): BSet[String] = walkBSet(cont, walk)

  /** AST value types */
  def walkAst(ast: BSet[String]): BSet[String] = walkBSet(ast, walk)

  /** grammar types */
  def walkGrammar(grammar: BSet[Grammar]): BSet[Grammar] =
    walkBSet(grammar, walk)

  /** grammar */
  def walk(grammar: Grammar): Grammar = Grammar(
    walk(grammar.name),
    walkList(grammar.params, walk),
  )

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Boolean = walk(codeUnit)

  /** constant types */
  def walkConst(const: Set[String]): Set[String] = walkSet(const, walk)

  /** mathematical value types */
  def walkMath(math: Boolean): Boolean = walk(math)

  /** number types */
  def walkNumber(number: Boolean): Boolean = walk(number)

  /** big integer types */
  def walkBigInt(bigInt: Boolean): Boolean = walk(bigInt)

  /** string types */
  def walkStr(str: BSet[String]): BSet[String] = walkBSet(str, walk)

  /** boolean types */
  def walkBool(bool: Set[Boolean]): Set[Boolean] = walkSet(bool, walk)

  /** undefined types */
  def walkUndef(undef: Boolean): Boolean = walk(undef)

  /** null types */
  def walkNull(nullv: Boolean): Boolean = walk(nullv)

  /** absent types */
  def walkAbsent(absent: Boolean): Boolean = walk(absent)

  /** name types */
  def walkNames(names: Set[String]): Set[String] = walkSet(names, walk)

  /** record types */
  def walk(ty: RecordTy): RecordTy = RecordTy(
    walkMap(ty.map, walk, walkOpt(_, walk)),
  )

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
