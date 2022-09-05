package esmeta.ty

import esmeta.cfg.Func
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
def CompT(normal: ValueTy, abrupt: Boolean): ValueTy =
  ValueTy(normal = normal.pureValue, abrupt = abrupt)
val AbruptT: ValueTy = ValueTy(abrupt = true)
def NormalT(value: ValueTy): ValueTy = ValueTy(normal = value.pureValue)
def SubMapT(key: ValueTy, value: ValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key.pureValue, value.pureValue))
def SubMapT(key: PureValueTy, value: PureValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key, value))
val CloTopT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy = ValueTy(clo = Fin(names.toSet))
val ContTopT: ValueTy = ValueTy(cont = Inf)
def ContT(names: String*): ValueTy = ValueTy(cont = Fin(names.toSet))
val ESPureValueT: PureValueTy = PureValueTy(
  names = Set("Object"),
  symbol = true,
  number = true,
  bigInt = true,
  str = Inf,
  bool = Set(true, false),
  undef = true,
  nullv = true,
)
val ESValueT: ValueTy = ValueTy(pureValue = ESPureValueT)
def NameT(names: String*): ValueTy = ValueTy(names = names.toSet)
val ObjectT: ValueTy = NameT("Object")
def RecordT(fields: Set[String]): ValueTy =
  ValueTy(record = RecordTy(fields))
def RecordT(map: Map[String, Option[ValueTy]]): ValueTy =
  ValueTy(record = RecordTy(map).norm)
def RecordT(pairs: (String, Option[ValueTy])*): ValueTy =
  ValueTy(record = RecordTy(pairs.toMap).norm)
def NilT: ValueTy = ValueTy(list = ListTy(Some(BotT)))
def ListT(ty: ValueTy): ValueTy = ValueTy(list = ListTy(Some(ty)))
val SymbolT: ValueTy = ValueTy(symbol = true)
val AstTopT: ValueTy = ValueTy(astValue = Inf)
def AstT(xs: String*): ValueTy = ValueTy(astValue = Fin(xs.toSet))
def GrammarT(xs: Grammar*): ValueTy = ValueTy(grammar = Fin(xs.toSet))
val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def ConstT(xs: String*): ValueTy = ValueTy(const = xs.toSet)
val MathT: ValueTy = ValueTy(math = true)
val NumberT: ValueTy = ValueTy(number = true)
val BigIntT: ValueTy = ValueTy(bigInt = true)
val StrTopT: ValueTy = ValueTy(str = Inf)
def StrT(set: Set[String]): ValueTy = ValueTy(str = Fin(set))
def StrT(xs: String*): ValueTy = ValueTy(str = Fin(xs.toSet))
def BoolT(set: Set[Boolean]): ValueTy = ValueTy(bool = set)
def BoolT(seq: Boolean*): ValueTy = ValueTy(bool = seq.toSet)
val BoolT: ValueTy = BoolT(true, false)
val TrueT: ValueTy = BoolT(true)
val FalseT: ValueTy = BoolT(false)
val UndefT: ValueTy = ValueTy(undef = true)
val NullT: ValueTy = ValueTy(nullv = true)
val AbsentT: ValueTy = ValueTy(absent = true)
val BotT: ValueTy = ValueTy()

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
  def getSingle[U >: T]: Flat[U] = elem.size match
    case 0 => Zero
    case 1 => One(elem.head)
    case _ => Many
}
