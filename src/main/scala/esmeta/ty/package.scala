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
val AbruptT: ValueTy = ValueTy(abrupt = true)
def NormalT(value: PureValueTy): ValueTy = ValueTy(normal = value)
def SubMapT(key: ValueTy, value: ValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key.pureValue, value.pureValue))
def SubMapT(key: PureValueTy, value: PureValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key, value))
val CloTopT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy = ValueTy(clo = Fin(names.toSet))
val ContTopT: ValueTy = ValueTy(cont = Inf)
def ContT(names: String*): ValueTy = ValueTy(cont = Fin(names.toSet))
val ESPureValueT: PureValueTy = PureValueTy(
  record = RecordTy(Set("Object")),
  number = true,
  bigInt = true,
  str = Inf,
  bool = Set(true, false),
  undef = true,
  nullv = true,
  absent = true,
)
val ESValueT: ValueTy = ValueTy(pureValue = ESPureValueT)
def RecordT(xs: String*): ValueTy =
  ValueTy(record = RecordTy(names = xs.toSet))
def RecordT(
  names: Set[String],
  fields: Set[String],
  map: Map[String, ValueTy],
): ValueTy = ValueTy(record = RecordTy(names, fields, map).norm)
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
def StrT(xs: String*): ValueTy = ValueTy(str = Fin(xs.toSet))
val BoolT: ValueTy = ValueTy(bool = Set(true, false))
val UndefT: ValueTy = ValueTy(undef = true)
val NullT: ValueTy = ValueTy(nullv = true)
val AbsentT: ValueTy = ValueTy(absent = true)
val BotT: ValueTy = ValueTy()

extension (elem: Boolean) {
  def isBottom: Boolean = elem == false
  def --(that: Boolean): Boolean = elem && !that
}
extension [T](elem: Set[T]) {
  def isBottom: Boolean = elem.isEmpty
  def <=(that: Set[T]): Boolean = elem subsetOf that
}
extension [A, B <: Lattice[B]](elem: Map[A, B]) {
  def isBottom: Boolean = elem.isEmpty
  def <=(that: Map[A, B]): Boolean = (for {
    key <- (elem.keySet | that.keySet).toList
    bool = (elem.get(key), that.get(key)) match
      case (None, _)          => true
      case (_, None)          => false
      case (Some(l), Some(r)) => l <= r
  } yield bool).forall(_ == true)
  def |(that: Map[A, B]): Map[A, B] = (for {
    key <- (elem.keySet | that.keySet).toList
    value <- (elem.get(key), that.get(key)) match
      case (None, r)          => r
      case (l, None)          => l
      case (Some(l), Some(r)) => Some(l | r)
  } yield key -> value).toMap
  def &(that: Map[A, B]): Map[A, B] = (for {
    key <- (elem.keySet & that.keySet).toList
    value <- (elem.get(key), that.get(key)) match
      case (None, _) | (_, None) => None
      case (Some(l), Some(r))    => Some(l & r)
  } yield key -> value).toMap
  def --(that: Map[A, B]): Map[A, B] = (for {
    key <- elem.keySet.toList
    value <- (elem.get(key), that.get(key)) match
      case (None, _)          => None
      case (l, None)          => l
      case (Some(l), Some(r)) => Some(l -- r)
  } yield key -> value).toMap
}
