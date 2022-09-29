package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.state.*
import esmeta.util.*

/** value types */
case class ValueTy(
  comp: CompTy,
  pureValue: PureValueTy,
  subMap: SubMapTy,
) extends Ty
  with Lattice[ValueTy] {
  import ValueTy.*

  /** top check */
  def isTop: Boolean =
    if (this eq Top) true
    else if (this eq Bot) false
    else
      (
        this.comp.isTop &&
        this.pureValue.isTop &&
        this.subMap.isTop
      )

  /** bottom check */
  def isBottom: Boolean =
    if (this eq Bot) true
    else if (this eq Top) false
    else
      (
        this.comp.isBottom &&
        this.pureValue.isBottom &&
        this.subMap.isBottom
      )

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean = (this eq that) || (
    this.comp <= that.comp &&
    this.pureValue <= that.pureValue &&
    this.subMap <= that.subMap
  )

  /** union type */
  def ||(that: => ValueTy): ValueTy =
    if (this eq that) this
    else
      ValueTy(
        this.comp || that.comp,
        this.pureValue || that.pureValue,
        this.subMap || that.subMap,
      )

  /** intersection type */
  def &&(that: => ValueTy): ValueTy =
    if (this eq that) this
    else
      ValueTy(
        this.comp && that.comp,
        this.pureValue && that.pureValue,
        this.subMap && that.subMap,
      )

  /** prune type */
  def --(that: => ValueTy): ValueTy =
    if (that.isBottom) this
    else
      ValueTy(
        this.comp -- that.comp,
        this.pureValue -- that.pureValue,
        this.subMap -- that.subMap,
      )

  /** get single value */
  def getSingle: Flat[AValue] =
    this.comp.getSingle ||
    this.pureValue.getSingle ||
    this.subMap.getSingle

  /** completion check */
  def isCompletion: Boolean =
    !comp.isBottom &&
    pureValue.isBottom &&
    subMap.isBottom

  /** getters */
  def normal: PureValueTy = comp.normal
  def abrupt: BSet[String] = comp.abrupt
  def clo: BSet[String] = pureValue.clo
  def cont: BSet[Int] = pureValue.cont
  def name: NameTy = pureValue.name
  def record: RecordTy = pureValue.record
  def list: ListTy = pureValue.list
  def symbol: Boolean = pureValue.symbol
  def astValue: AstValueTy = pureValue.astValue
  def nt: BSet[Nt] = pureValue.nt
  def codeUnit: Boolean = pureValue.codeUnit
  def const: BSet[String] = pureValue.const
  def math: BSet[BigDecimal] = pureValue.math
  def number: BSet[Number] = pureValue.number
  def bigInt: Boolean = pureValue.bigInt
  def str: BSet[String] = pureValue.str
  def bool: BoolTy = pureValue.bool
  def undef: Boolean = pureValue.undef
  def nullv: Boolean = pureValue.nullv
  def absent: Boolean = pureValue.absent

  /** value containment check */
  def contains(value: Value, state: State): Boolean = value match
    case Comp(ty, value, target) => isCompletion
    // address 굳이 두개로 나누지 않아도 됨.
    case a: Addr =>
      state.heap(a) match {
        case m @ MapObj(ty, props, _) =>
          (names contains ty) | m.keys.foldLeft(true)({
            case (t, key) => (
              t && (record.map.get(key.asStr) match
                case None => false
                case Some(v) =>
                  v match
                    case None    => true
                    case Some(v) => v.contains(props.get(key).get.value, state)
              )
            )
          }) // MapObj 는 purevalue -> props 이고 record type은 string -> valueTy 인데 key.asStr 으로 넘길 수 있는지..
        // or named record 즉 (mapobj의 ty 가 names 안에 존재한다면) 면 name 안에 들어갈 시 그냥 바로 true retur면
        // 아닐경우 mapobj 의 pairs 를 가져와서 record의 keyset의 subset인지 확인하고 각 mapobj 의 value가 record에서 지정된 type 안에 들어가는지 체크
        // record에서 value type은 none일 경우 bottom Some(none)일 경우 Top
        // 각 value in mapobj pair 가 record의 valuetype에 들어가는 지 체크해주어야함.
        case ListObj(values) =>
          list.elem match
            case None => false
            case Some(ty) =>
              values.foldLeft(true)({
                case (t, v) => t && (ty.contains(v, state))
              })

        // ListObj(p) 에서 p에 각 element 마다 for each 로 check 다 해주어야함
        // list 로 가져온거중에 elem 이 none 일 경우 아무것도 올 수 없다.
        // list 로 가져온거중에 elem 이 Some(type)인 경우 for each 로 ListObj(p)에 p를 iterate 해야함
        case SymbolObj(_) => symbol
        case YetObj(_, _) => true
      }
    case Clo(func, captured)             => clo contains func.irFunc.name
    case Cont(func, captured, callStack) => cont contains func.id
    // why does clo return BSet[String] and why does cont return BSet[Int]
    case AstValue(ast) =>
      astValue match
        case AstTopTy       => true
        case a: AstNonTopTy => a.toName.names contains ast.name
    case g @ Grammar(name, params) => grammar contains g
    case Math(n)                   => math contains n
    case Const(name)               => const contains name
    case CodeUnit(c)               => codeUnit
    case num @ Number(n)           => number contains num
    case BigInt(n)                 => bigInt
    case Str(s)                    => str contains s
    case Bool(b)                   => bool contains b
    case Undef                     => undef
    case Null                      => nullv
    case Absent                    => absent
}
object ValueTy {
  def apply(
    comp: CompTy = CompTy.Bot,
    normal: PureValueTy = PureValueTy.Bot,
    abrupt: BSet[String] = Fin(),
    pureValue: PureValueTy = PureValueTy.Bot,
    clo: BSet[String] = Fin(),
    cont: BSet[Int] = Fin(),
    name: NameTy = NameTy.Bot,
    record: RecordTy = RecordTy.Bot,
    list: ListTy = ListTy.Bot,
    symbol: Boolean = false,
    astValue: AstValueTy = AstValueTy.Bot,
    nt: BSet[Nt] = Fin(),
    codeUnit: Boolean = false,
    const: BSet[String] = Fin(),
    math: BSet[BigDecimal] = Fin(),
    number: BSet[Number] = Fin(),
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: BoolTy = BoolTy.Bot,
    undef: Boolean = false,
    nullv: Boolean = false,
    absent: Boolean = false,
    subMap: SubMapTy = SubMapTy.Bot,
  ): ValueTy = ValueTy(
    comp = comp || CompTy(normal, abrupt),
    pureValue = pureValue || PureValueTy(
      clo,
      cont,
      name,
      record,
      list,
      symbol,
      astValue,
      nt,
      codeUnit,
      const,
      math,
      number,
      bigInt,
      str,
      bool,
      undef,
      nullv,
      absent,
    ),
    subMap = subMap,
  )
  val Top: ValueTy = ValueTy(CompTy.Top, PureValueTy.Top, SubMapTy.Top)
  val Bot: ValueTy = ValueTy()
}
