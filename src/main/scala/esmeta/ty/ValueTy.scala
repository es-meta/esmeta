package esmeta.ty

import esmeta.cfg.{CFG, Func}
import esmeta.state.*
import esmeta.util.*
import esmeta.ty.util.Parser

/** value types */
case class ValueTy(
  comp: CompTy,
  pureValue: PureValueTy,
) extends Ty
  with Lattice[ValueTy] {
  import ValueTy.*
  import ManualInfo.tyModel.{isSubTy, getBase}

  /** top check */
  def isTop: Boolean =
    if (this eq Top) true
    else if (this eq Bot) false
    else this.comp.isTop && this.pureValue.isTop

  /** bottom check */
  def isBottom: Boolean =
    if (this eq Bot) true
    else if (this eq Top) false
    else this.comp.isBottom && this.pureValue.isBottom

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean =
    (this eq that) ||
    (this.comp <= that.comp && this.pureValue <= that.pureValue)

  /** union type */
  def ||(that: => ValueTy): ValueTy =
    if (this eq that) this
    else ValueTy(this.comp || that.comp, this.pureValue || that.pureValue)

  /** intersection type */
  def &&(that: => ValueTy): ValueTy =
    if (this eq that) this
    else ValueTy(this.comp && that.comp, this.pureValue && that.pureValue)

  /** prune type */
  def --(that: => ValueTy): ValueTy =
    if (that.isBottom) this
    else ValueTy(this.comp -- that.comp, this.pureValue -- that.pureValue)

  /** completion check */
  def isCompletion: Boolean = !comp.isBottom && pureValue.isBottom

  /** remove absent types */
  def removeAbsent: ValueTy = copy(
    comp = CompTy(normal.removeAbsent, abrupt),
    pureValue = pureValue.removeAbsent,
  )

  /** getters */
  def normal: PureValueTy = comp.normal
  def abrupt: BSet[String] = comp.abrupt
  def clo: BSet[String] = pureValue.clo
  def cont: BSet[Int] = pureValue.cont
  def record: RecordTy = pureValue.record
  def map: MapTy = pureValue.map
  def list: ListTy = pureValue.list
  def ast: AstTy = pureValue.ast
  def grammarSymbol: BSet[GrammarSymbol] = pureValue.grammarSymbol
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
          case obj: RecordObj => record.contains(obj, heap)
          case obj: MapObj    => map.contains(obj, heap)
          case obj: ListObj   => list.contains(obj, heap)
          case obj: YetObj    => true
      case Clo(func, captured)             => clo contains func.irFunc.name
      case Cont(func, captured, callStack) => cont contains func.id
      case v: AstValue                     => ast.contains(v)
      case x @ GrammarSymbol(name, params) => grammarSymbol contains x
      case m: Math                         => math contains m
      case Infinity(p)                     => infinity contains p
      case Enum(name)                      => enumv contains name
      case CodeUnit(c)                     => codeUnit
      case n: Number                       => number contains n
      case BigInt(n)                       => bigInt
      case Str(s)                          => str contains s
      case Bool(b)                         => bool contains b
      case Undef                           => undef
      case Null                            => nullv
      case Absent                          => absent
    )

  /** copy value type */
  def copied(
    comp: CompTy = CompTy.Bot,
    normal: PureValueTy = normal,
    abrupt: BSet[String] = abrupt,
    pureValue: PureValueTy = PureValueTy.Bot,
    clo: BSet[String] = clo,
    cont: BSet[Int] = cont,
    record: RecordTy = record,
    map: MapTy = map,
    list: ListTy = list,
    ast: AstTy = ast,
    grammarSymbol: BSet[GrammarSymbol] = grammarSymbol,
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
  ): ValueTy = ValueTy(
    comp = comp || CompTy(normal, abrupt),
    pureValue = pureValue || PureValueTy(
      clo,
      cont,
      record,
      map,
      list,
      ast,
      grammarSymbol,
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
  )

  /** get single value */
  def getSingle: Flat[Value] =
    this.comp.getSingle ||
    this.pureValue.getSingle ||
    this.map.getSingle

  /** types having no field */
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
    record: RecordTy = RecordTy.Bot,
    map: MapTy = MapTy.Bot,
    list: ListTy = ListTy.Bot,
    ast: AstTy = AstTy.Bot,
    grammarSymbol: BSet[GrammarSymbol] = Fin(),
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
  ): ValueTy = ValueTy(
    comp = comp || CompTy(normal, abrupt),
    pureValue = pureValue || PureValueTy(
      clo,
      cont,
      record,
      map,
      list,
      ast,
      grammarSymbol,
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
  )
  lazy val Top: ValueTy = ValueTy(CompTy.Top, PureValueTy.Top)
  lazy val Bot: ValueTy = ValueTy()
}
