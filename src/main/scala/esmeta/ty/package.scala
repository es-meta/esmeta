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
val CompT: ValueTy = ValueTy(normal = None, abrupt = true)
def CompT(normal: ValueTy, abrupt: Boolean): ValueTy =
  if (normal.pureValue.isBottom && !abrupt) ValueTy.Bot
  else ValueTy(normal = Some(normal.pureValue), abrupt = abrupt)
val AbruptT: ValueTy = ValueTy(abrupt = true)
def NormalT(value: ValueTy): ValueTy =
  if (value.pureValue.isBottom) ValueTy.Bot
  else ValueTy(normal = Some(value.pureValue))
def SubMapT(key: ValueTy, value: ValueTy): ValueTy =
  if (key.isBottom || value.isBottom) ValueTy.Bot
  else ValueTy(subMap = SubMapTy(key.pureValue, value.pureValue))
def SubMapT(key: PureValueTy, value: PureValueTy): ValueTy =
  if (key.isBottom || value.isBottom) ValueTy.Bot
  else ValueTy(subMap = SubMapTy(key, value))
val CloTopT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy =
  if (names.isEmpty) ValueTy.Bot
  else ValueTy(clo = Fin(names.toSet))
val ContTopT: ValueTy = ValueTy(cont = Inf)
def ContT(nids: Int*): ValueTy =
  if (nids.isEmpty) ValueTy.Bot
  else ValueTy(cont = Fin(nids.toSet))
def NameT(names: String*): ValueTy =
  if (names.isEmpty) ValueTy.Bot
  else ValueTy(name = NameTy(names.toSet))
val ObjectT: ValueTy = NameT("Object")
val ESPrimT: ValueTy = ValueTy(
  symbol = true,
  number = Inf,
  bigInt = true,
  str = Inf,
  bool = Set(true, false),
  undef = true,
  nullv = true,
)
val ESValueT: ValueTy = ObjectT || ESPrimT
val ESPureValueT: PureValueTy = ESValueT.pureValue
def RecordT(fields: Set[String]): ValueTy =
  if (fields.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(fields))
def RecordT(map: Map[String, Option[ValueTy]]): ValueTy =
  if (map.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(map).norm)
def RecordT(pairs: (String, Option[ValueTy])*): ValueTy =
  if (pairs.isEmpty) ValueTy.Bot
  else ValueTy(record = RecordTy(pairs.toMap).norm)
def NilT: ValueTy = ValueTy(list = ListTy(Some(BotT)))
def ListT(ty: ValueTy): ValueTy =
  if (ty.isBottom) ValueTy.Bot
  else ValueTy(list = ListTy(Some(ty)))
val SymbolT: ValueTy = ValueTy(symbol = true)
val AstTopT: ValueTy = ValueTy(astValue = AstTopTy)
def AstT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(astValue = AstNameTy(xs.toSet))
def AstSingleT(name: String, idx: Int, subIdx: Int): ValueTy =
  ValueTy(astValue = AstSingleTy(name, idx, subIdx))
def NtT(xs: Nt*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(nt = Fin(xs.toSet))
val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def ConstT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(const = xs.toSet)
val MathTopT: ValueTy = ValueTy(math = Inf)
def MathT(ns: BigDecimal*): ValueTy =
  if (ns.isEmpty) ValueTy.Bot
  else ValueTy(math = Fin(ns.toSet))
val NumberTopT: ValueTy = ValueTy(number = Inf)
def NumberT(ns: Number*): ValueTy =
  if (ns.isEmpty) ValueTy.Bot
  else ValueTy(number = Fin(ns.toSet))
val BigIntT: ValueTy = ValueTy(bigInt = true)
val StrTopT: ValueTy = ValueTy(str = Inf)
def StrT(set: Set[String]): ValueTy =
  if (set.isEmpty) ValueTy.Bot
  else ValueTy(str = Fin(set))
def StrT(xs: String*): ValueTy =
  if (xs.isEmpty) ValueTy.Bot
  else ValueTy(str = Fin(xs.toSet))
def BoolT(set: Set[Boolean]): ValueTy =
  if (set.isEmpty) ValueTy.Bot
  else ValueTy(bool = set)
def BoolT(seq: Boolean*): ValueTy =
  if (seq.isEmpty) ValueTy.Bot
  else ValueTy(bool = seq.toSet)
val BoolT: ValueTy = BoolT(true, false)
val TrueT: ValueTy = BoolT(true)
val FalseT: ValueTy = BoolT(false)
val UndefT: ValueTy = ValueTy(undef = true)
val NullT: ValueTy = ValueTy(nullv = true)
val AbsentT: ValueTy = ValueTy(absent = true)
val BotT: ValueTy = ValueTy.Bot

/** predefined constant types */
val CONSTT_EMPTY = ConstT("empty")
val CONSTT_UNRESOLVABLE = ConstT("unresolvable")
val CONSTT_LEXICAL = ConstT("lexical")
val CONSTT_INITIALIZED = ConstT("initialized")
val CONSTT_UNINITIALIZED = ConstT("uninitialized")
val CONSTT_BASE = ConstT("base")
val CONSTT_DERIVED = ConstT("derived")
val CONSTT_STRICT = ConstT("strict")
val CONSTT_GLOBAL = ConstT("global")
val CONSTT_UNLINKED = ConstT("unlinked")
val CONSTT_LINKING = ConstT("linking")
val CONSTT_LINKED = ConstT("linked")
val CONSTT_EVALUATING = ConstT("evaluating")
val CONSTT_EVALUATED = ConstT("evaluated")
val CONSTT_NUMBER = ConstT("Number")
val CONSTT_BIGINT = ConstT("BigInt")
val CONSTT_NORMAL = ConstT("normal")
val CONSTT_BREAK = ConstT("break")
val CONSTT_CONTINUE = ConstT("continue")
val CONSTT_RETURN = ConstT("return")
val CONSTT_THROW = ConstT("throw")
val CONSTT_SUSPENDED_START = ConstT("suspendedStart")
val CONSTT_SUSPENDED_YIELD = ConstT("suspendedYield")
val CONSTT_EXECUTING = ConstT("executing")
val CONSTT_AWAITING_RETURN = ConstT("awaitingDASHreturn")
val CONSTT_COMPLETED = ConstT("completed")
val CONSTT_PENDING = ConstT("pending")
val CONSTT_FULFILLED = ConstT("fulfilled")
val CONSTT_REJECTED = ConstT("rejected")
val CONSTT_FULFILL = ConstT("Fulfill")
val CONSTT_REJECT = ConstT("Reject")

extension (elem: Boolean) {
  def isBottom: Boolean = elem == false
  def --(that: Boolean): Boolean = elem && !that
}
extension [T](elem: Set[T]) {
  def isBottom: Boolean = elem.isEmpty
  def <=(that: Set[T]): Boolean = elem subsetOf that
  def ||(that: Set[T]): Set[T] = elem ++ that
  def &&(that: Set[T]): Set[T] = elem intersect that
  def getSingle[U >: T]: Flat[U] = elem.size match
    case 0 => Zero
    case 1 => One(elem.head)
    case _ => Many
}
