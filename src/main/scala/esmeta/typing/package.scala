package esmeta.typing

import esmeta.ir.Name
import esmeta.state.*
import esmeta.typing.util.*
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
def SubMapT(key: ValueTy, value: ValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key.pureValue, value.pureValue))
def SubMapT(key: PureValueTy, value: PureValueTy): ValueTy =
  ValueTy(subMap = SubMapTy(key, value))
def CloT(method: String, caputred: Map[Name, Ty] = Map()): ValueTy =
  ValueTy(clo = CloTy(method -> CapturedMapTy()))
val ESValueT: ValueTy = ValueTy(
  record = RecordTy(Set("Object")),
  number = true,
  bigInt = true,
  str = Inf,
  bool = Set(true, false),
  undef = true,
  nullv = true,
  absent = true,
)
def RecordT(xs: String*): ValueTy = ValueTy(record = RecordTy(xs.toSet))
def ListT(ty: ValueTy): ValueTy = ValueTy(list = ListTy(Some(ty)))
val SymbolT: ValueTy = ValueTy(symbol = true)
val AstTopT: ValueTy = ValueTy(astValue = Inf)
def AstT(xs: String*): ValueTy = ValueTy(astValue = Fin(xs.toSet))
def GrammarT(xs: Grammar*): ValueTy = ValueTy(grammar = Fin(xs.toSet))
val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def ConstT(xs: String*): ValueTy = ValueTy(const = xs.toSet)
val MathT: ValueTy = ValueTy(bigInt = true)
val NumberT: ValueTy = ValueTy(bigInt = true)
val BigIntT: ValueTy = ValueTy(bigInt = true)
val StrT: ValueTy = ValueTy(str = Inf)
val BoolT: ValueTy = ValueTy(bool = Set(true, false))
val UndefT: ValueTy = ValueTy(undef = true)
val NullT: ValueTy = ValueTy(nullv = true)
val AbsentT: ValueTy = ValueTy(absent = true)
val BotT: ValueTy = ValueTy()

extension (elem: Boolean) { def --(that: Boolean): Boolean = elem && !that }
