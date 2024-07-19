package esmeta.ty

import esmeta.cfg.Node
import esmeta.state.*
import esmeta.ty.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type elements */
trait TyElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}

// -----------------------------------------------------------------------------
// helpers
// -----------------------------------------------------------------------------
lazy val AnyT: ValueTy = ValueTy.Top
lazy val PureValueT: ValueTy = ValueTy(pureValue = PureValueTy.Top)
lazy val CompT: ValueTy = ValueTy(comp = CompTy.Top)
def CompT(normal: ValueTy, abrupt: BSet[String]): ValueTy =
  if (normal.pureValue.isBottom && abrupt.isBottom) ValueTy.Bot
  else ValueTy(normal = normal.pureValue, abrupt = abrupt)
lazy val AbruptT: ValueTy = ValueTy(abrupt = Inf)
def AbruptT(names: String*): ValueTy = ValueTy(abrupt = Fin(names: _*))
lazy val NormalT: ValueTy = ValueTy(normal = PureValueTy.Top)
def NormalT(value: ValueTy): ValueTy =
  if (value.pureValue.isBottom) ValueTy.Bot
  else ValueTy(normal = value.pureValue)
def SubMapT: ValueTy = ValueTy(subMap = SubMapTy.Top)
def SubMapT(key: ValueTy, value: ValueTy): ValueTy =
  if (key.isBottom || value.isBottom) ValueTy.Bot
  else ValueTy(subMap = SubMapTy(key.pureValue, value.pureValue))
def SubMapT(key: PureValueTy, value: PureValueTy): ValueTy =
  if (key.isBottom || value.isBottom) ValueTy.Bot
  else ValueTy(subMap = SubMapTy(key, value))
lazy val CloT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy =
  if (names.isEmpty) ValueTy.Bot
  else ValueTy(clo = Fin(names.toSet))
lazy val ContT: ValueTy = ValueTy(cont = Inf)
def ContT(nids: Int*): ValueTy =
  if (nids.isEmpty) ValueTy.Bot
  else ValueTy(cont = Fin(nids.toSet))
lazy val NameT: ValueTy = ValueTy(name = NameTy.Top)
def NameT(names: String*): ValueTy = NameT(names.toSet)
def NameT(set: Set[String]): ValueTy =
  if (set.isEmpty) ValueTy.Bot
  else ValueTy(name = NameTy(Fin(set)))
lazy val ObjectT: ValueTy = NameT("Object")
lazy val FunctionT: ValueTy = NameT("FunctionObject")
lazy val ConstructorT: ValueTy = NameT("Constructor")
lazy val ESPrimT: ValueTy = ValueTy(
  symbol = true,
  number = NumberTy.Top,
  bigInt = true,
  str = Inf,
  bool = BoolTy.Top,
  undef = true,
  nullv = true,
)
lazy val ESValueT: ValueTy = ObjectT || ESPrimT
lazy val ESPureValueT: PureValueTy = ESValueT.pureValue
lazy val RecordT: ValueTy = ValueTy(record = RecordTy.Top)
def RecordT(fields: Set[String]): ValueTy =
  if (fields.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(fields))
def RecordT(map: Map[String, ValueTy]): ValueTy =
  if (map.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(map).normalized)
def RecordT(pairs: (String, ValueTy)*): ValueTy =
  if (pairs.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(pairs.toMap).normalized)
def NilT: ValueTy = ValueTy(list = ListTy(Some(BotT)))
def ListT: ValueTy = ValueTy(list = ListTy.Top)
def ListT(ty: ValueTy): ValueTy =
  if (ty.isBottom) ValueTy.Bot
  else ValueTy(list = ListTy(Some(ty)))
lazy val SymbolT: ValueTy = ValueTy(symbol = true)
lazy val AstT: ValueTy = ValueTy(astValue = AstTopTy)
def AstT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(astValue = AstNameTy(xs.toSet))
def AstSingleT(name: String, idx: Int, subIdx: Int): ValueTy =
  ValueTy(astValue = AstSingleTy(name, idx, subIdx))
def NtT: ValueTy = ValueTy(nt = Inf)
def NtT(xs: Nt*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(nt = Fin(xs.toSet))
lazy val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def EnumT: ValueTy = ValueTy(enumv = Inf)
def EnumT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(enumv = Fin(xs.toSet))
lazy val MathT: ValueTy = ValueTy(math = MathTy.Top)
lazy val ExtMathT: ValueTy = MathT || InfinityT
lazy val IntT: ValueTy = ValueTy(math = IntTy)
lazy val NonPosIntT: ValueTy = ValueTy(math = NonPosIntTy)
lazy val NonNegIntT: ValueTy = ValueTy(math = NonNegIntTy)
lazy val NegIntT: ValueTy = ValueTy(math = NegIntTy)
lazy val PosIntT: ValueTy = ValueTy(math = PosIntTy)
def MathT(ds: BigDecimal*): ValueTy =
  if (ds.isEmpty) ValueTy.Bot
  else ValueTy(math = MathSetTy(ds.toSet.map(Math(_))))
lazy val InfinityT: ValueTy = ValueTy(infinity = InfinityTy.Top)
lazy val NegInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Neg)
lazy val PosInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Pos)
def InfinityT(ps: Boolean*): ValueTy =
  if (ps.isEmpty) ValueTy.Bot
  else ValueTy(infinity = InfinityTy(ps.toSet))
lazy val NumericT: ValueTy = NumberT || BigIntT
lazy val NumberT: ValueTy = ValueTy(number = NumberTy.Top)
lazy val NumberIntT: ValueTy = ValueTy(number = NumberTy.Int)
def NumberT(ns: Number*): ValueTy =
  if (ns.isEmpty) ValueTy.Bot
  else ValueTy(number = NumberSetTy(ns.toSet))
lazy val BigIntT: ValueTy = ValueTy(bigInt = true)
lazy val StrT: ValueTy = ValueTy(str = Inf)
def StrT(set: Set[String]): ValueTy =
  if (set.isEmpty) ValueTy.Bot
  else ValueTy(str = Fin(set))
def StrT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(str = Fin(xs.toSet))
def BoolT(set: Set[Boolean]): ValueTy =
  if (set.isEmpty) ValueTy.Bot
  else ValueTy(bool = BoolTy(set))
def BoolT(seq: Boolean*): ValueTy =
  if (seq.isEmpty) ValueTy.Bot
  else ValueTy(bool = BoolTy(seq.toSet))
lazy val BoolT: ValueTy = BoolT(true, false)
lazy val TrueT: ValueTy = BoolT(true)
lazy val FalseT: ValueTy = BoolT(false)
lazy val UndefT: ValueTy = ValueTy(undef = true)
lazy val NullT: ValueTy = ValueTy(nullv = true)
lazy val AbsentT: ValueTy = ValueTy(absent = true)
lazy val BotT: ValueTy = ValueTy.Bot

/** predefined enum types */
val ENUMT_EMPTY = EnumT("empty")
val ENUMT_UNRESOLVABLE = EnumT("unresolvable")
val ENUMT_LEXICAL = EnumT("lexical")
val ENUMT_INITIALIZED = EnumT("initialized")
val ENUMT_UNINITIALIZED = EnumT("uninitialized")
val ENUMT_BASE = EnumT("base")
val ENUMT_DERIVED = EnumT("derived")
val ENUMT_STRICT = EnumT("strict")
val ENUMT_GLOBAL = EnumT("global")
val ENUMT_UNLINKED = EnumT("unlinked")
val ENUMT_LINKING = EnumT("linking")
val ENUMT_LINKED = EnumT("linked")
val ENUMT_EVALUATING = EnumT("evaluating")
val ENUMT_EVALUATED = EnumT("evaluated")
val ENUMT_NUMBER = EnumT("Number")
val ENUMT_BIGINT = EnumT("BigInt")
val ENUMT_NORMAL = EnumT("normal")
val ENUMT_BREAK = EnumT("break")
val ENUMT_CONTINUE = EnumT("continue")
val ENUMT_RETURN = EnumT("return")
val ENUMT_THROW = EnumT("throw")
val ENUMT_SUSPENDED_START = EnumT("suspendedStart")
val ENUMT_SUSPENDED_YIELD = EnumT("suspendedYield")
val ENUMT_EXECUTING = EnumT("executing")
val ENUMT_AWAITING_RETURN = EnumT("awaitingDASHreturn")
val ENUMT_COMPLETED = EnumT("completed")
val ENUMT_PENDING = EnumT("pending")
val ENUMT_FULFILLED = EnumT("fulfilled")
val ENUMT_REJECTED = EnumT("rejected")
val ENUMT_FULFILL = EnumT("Fulfill")
val ENUMT_REJECT = EnumT("Reject")

extension (elem: Boolean) {
  def isTop: Boolean = elem == true
  def isBottom: Boolean = elem == false
  def --(that: Boolean): Boolean = elem && !that
}
