package esmeta.ty.util

import esmeta.state.{GrammarSymbol, Number, Math}
import esmeta.ty.*
import esmeta.util.*

/** a unit walker for types */
trait UnitWalker extends BasicUnitWalker {

  /** type elements */
  def walk(ty: TyElem): Unit = ty match
    case elem: TyModel     => walk(elem)
    case elem: TyDecl      => walk(elem)
    case elem: TyDecl.Elem => walk(elem)
    case elem: FieldMap    => walk(elem)
    case elem: Ty          => walk(elem)
    case elem: RecordTy    => walk(elem)
    case elem: ListTy      => walk(elem)
    case elem: AstTy       => walk(elem)
    case elem: MapTy       => walk(elem)
    case elem: MathTy      => walk(elem)
    case elem: InfinityTy  => walk(elem)
    case elem: NumberTy    => walk(elem)
    case elem: BoolTy      => walk(elem)

  /** type models */
  def walk(ty: TyModel): Unit = walkList(ty.decls, walk)

  /** type declarations */
  def walk(ty: TyDecl): Unit =
    walk(ty.name)
    walkOpt(ty.parent, walk)
    walkList(ty.elems, walk)

  /** type declaration elements */
  def walk(ty: TyDecl.Elem): Unit =
    import TyDecl.Elem.*
    ty match
      case AbsMethod(name) =>
        walk(name)
      case ConMethod(name, optional, target) =>
        walk(name); walk(optional); walkOpt(target, walk)
      case Field(name, optional, typeStr) =>
        walk(name); walk(optional); walk(typeStr)

  /** type map */
  def walk(fieldMap: FieldMap): Unit = walkMap(fieldMap.map, walk, walk)

  /** types */
  def walk(ty: Ty): Unit = ty match
    case ty: UnknownTy => walk(ty)
    case ty: ValueTy   => walk(ty)

  /** unknown types */
  def walk(ty: UnknownTy): Unit =
    walkOpt(ty.msg, walk)

  /** value types */
  def walk(ty: ValueTy): Unit = if (!ty.isTop)
    walkClo(ty.clo)
    walkCont(ty.cont)
    walk(ty.record)
    walk(ty.map)
    walk(ty.list)
    walkAst(ty.ast)
    walkGrammarSymbol(ty.grammarSymbol)
    walkCodeUnit(ty.codeUnit)
    walkEnum(ty.enumv)
    walkMath(ty.math)
    walkInfinity(ty.infinity)
    walkNumber(ty.number)
    walkBigInt(ty.bigInt)
    walkStr(ty.str)
    walkBool(ty.bool)
    walkUndef(ty.undef)
    walkNull(ty.nullv)

  /** closure types */
  def walkClo(clo: BSet[String]): Unit = walkBSet(clo, walk)

  /** continuation types */
  def walkCont(cont: BSet[Int]): Unit = walkBSet(cont, walk)

  /** AST value types */
  def walkAst(ast: AstTy): Unit =
    import AstTy.*
    ast match
      case Top               =>
      case Simple(names)     => walkSet(names, walk)
      case Detail(name, idx) => walk(name); walk(idx)

  /** grammarSymbol types */
  def walkGrammarSymbol(grammarSymbol: BSet[GrammarSymbol]): Unit =
    walkBSet(grammarSymbol, walk)

  /** grammarSymbol */
  def walk(grammarSymbol: GrammarSymbol): Unit =
    walk(grammarSymbol.name)
    walkList(grammarSymbol.params, walk)

  /** code unit types */
  def walkCodeUnit(codeUnit: Boolean): Unit = walk(codeUnit)

  /** enum types */
  def walkEnum(enumv: BSet[String]): Unit = walkBSet(enumv, walk)

  /** mathematical value types */
  def walkMath(math: MathTy): Unit = math match
    case MathSetTy(set) => walkSet(set, walk)
    case _              =>
  def walk(math: Math): Unit = {}

  /** infinity types */
  def walkInfinity(infinity: InfinityTy): Unit = infinity.pos.map(walk)

  /** number types */
  def walkNumber(number: NumberTy): Unit = number match
    case NumberSetTy(set) => walkSet(set, walk)
    case _                =>
  def walk(number: Number): Unit = {}

  /** big integer types */
  def walkBigInt(bigInt: Boolean): Unit = walk(bigInt)

  /** string types */
  def walkStr(str: BSet[String]): Unit = walkBSet(str, walk)

  /** boolean types */
  def walkBool(bool: BoolTy): Unit = walkSet(bool.set, walk)

  /** undefined types */
  def walkUndef(undef: Boolean): Unit = walk(undef)

  /** null types */
  def walkNull(nullv: Boolean): Unit = walk(nullv)

  /** record types */
  def walk(ty: RecordTy): Unit =
    import RecordTy.*
    ty match
      case Top       =>
      case Elem(map) => walkMap(map, walk, walk)

  /** list types */
  def walk(ty: ListTy): Unit = ty match
    case ListTy.Elem(elem) => walk(elem)
    case _                 =>

  /** map types */
  def walk(ty: MapTy): Unit = ty match
    case MapTy.Elem(key, value) => walk(key); walk(value)
    case _                      =>
}
