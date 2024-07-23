package esmeta.ty

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.util.*
import esmeta.ty.util.Parser

/** value types */
case class ValueTy(
  comp: CompTy,
  pureValue: PureValueTy,
  subMap: SubMapTy,
) extends Ty
  with Lattice[ValueTy] {
  import ValueTy.*
  import ManualInfo.tyModel.isSubTy

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

  /** completion check */
  def isCompletion: Boolean =
    !comp.isBottom &&
    pureValue.isBottom &&
    subMap.isBottom

  /** remove absent types */
  def removeAbsent: ValueTy = copy(
    comp = CompTy(normal.removeAbsent, abrupt),
    pureValue = pureValue.removeAbsent,
    subMap = subMap,
  )

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
  def enumv: BSet[String] = pureValue.enumv
  def math: MathTy = pureValue.math
  def infinity: InfinityTy = pureValue.infinity
  def number: NumberTy = pureValue.number
  def bigInt: Boolean = pureValue.bigInt
  def str: BSet[String] = pureValue.str
  def bool: BoolTy = pureValue.bool
  def undef: Boolean = pureValue.undef
  def nullv: Boolean = pureValue.nullv
  def absent: Boolean = pureValue.absent

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean =
    (pureValue.isTop && value.isInstanceOf[PureValue]) || (value match
      case NormalComp(value) =>
        ValueTy(pureValue = comp.normal).contains(value, heap)
      case Comp(Enum(tyStr), _, _) => comp.abrupt contains tyStr
      case a: Addr =>
        heap(a) match
          case MapObj(tname, props, _) =>
            isSubTy(tname, name.set) ||
            (tname == "Record" && (props.forall {
              case (Str(key), MapObj.Field(value, _)) =>
                record(key).contains(value, heap)
              case _ => false
            })) ||
            (tname == "SubMap" && (props.forall {
              case (key, MapObj.Field(value, _)) =>
                ValueTy(pureValue = subMap.key).contains(key, heap) &&
                ValueTy(pureValue = subMap.value).contains(value, heap)
            }))
          case ListObj(values) =>
            list.elem match
              case None     => false
              case Some(ty) => values.forall(ty.contains(_, heap))
          case SymbolObj(_) => symbol
          case YetObj(_, _) => true
      case Clo(func, captured)             => clo contains func.irFunc.name
      case Cont(func, captured, callStack) => cont contains func.id
      case AstValue(ast) =>
        astValue match
          case AstTopTy         => true
          case AstNameTy(names) => names contains ast.name
          case AstSingleTy(name, idx, subIdx) =>
            ast.name == name &&
            ast.idx == idx &&
            ast.subIdx == subIdx
      case x @ Nt(name, params) => nt contains x
      case m: Math              => math contains m
      case Infinity(p)          => infinity contains p
      case Enum(name)           => enumv contains name
      case CodeUnit(c)          => codeUnit
      case n: Number            => number contains n
      case BigInt(n)            => bigInt
      case Str(s)               => str contains s
      case Bool(b)              => bool contains b
      case Undef                => undef
      case Null                 => nullv
      case Absent               => absent
    )

  /** copy value type */
  def copied(
    comp: CompTy = CompTy.Bot,
    normal: PureValueTy = normal,
    abrupt: BSet[String] = abrupt,
    pureValue: PureValueTy = PureValueTy.Bot,
    clo: BSet[String] = clo,
    cont: BSet[Int] = cont,
    name: NameTy = name,
    record: RecordTy = record,
    list: ListTy = list,
    symbol: Boolean = symbol,
    astValue: AstValueTy = astValue,
    nt: BSet[Nt] = nt,
    codeUnit: Boolean = codeUnit,
    enumv: BSet[String] = enumv,
    math: MathTy = math,
    infinity: InfinityTy = infinity,
    number: NumberTy = number,
    bigInt: Boolean = bigInt,
    str: BSet[String] = str,
    bool: BoolTy = bool,
    undef: Boolean = undef,
    nullv: Boolean = nullv,
    absent: Boolean = absent,
    subMap: SubMapTy = subMap,
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
      enumv,
      math,
      infinity,
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

  /** get single value */
  def getSingle: Flat[Value] =
    this.comp.getSingle ||
    this.pureValue.getSingle ||
    this.subMap.getSingle

  /** types having no property */
  def noField: ValueTy = Bot.copy(pureValue = pureValue.noField)
}
object ValueTy extends Parser.From(Parser.valueTy) {
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
    enumv: BSet[String] = Fin(),
    math: MathTy = MathTy.Bot,
    infinity: InfinityTy = InfinityTy.Bot,
    number: NumberTy = NumberTy.Bot,
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
      enumv,
      math,
      infinity,
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
  lazy val Top: ValueTy = ValueTy(CompTy.Top, PureValueTy.Top, SubMapTy.Top)
  lazy val Bot: ValueTy = ValueTy()
}
