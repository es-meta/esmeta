package esmeta.state

import esmeta.cfg.{CFG, Func}
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.DoubleEquals
import java.math.MathContext.UNLIMITED
import scala.collection.mutable.{Map => MMap}
import esmeta.util.ManualInfo.tyModel

/** IR values */
sealed trait Value extends StateElem {

  /** wrap completion */
  def wrapCompletion(using st: State, cfg: CFG): Addr = wrapCompletion(
    ENUM_NORMAL,
  )
  def wrapCompletion(ty: Enum)(using st: State, cfg: CFG): Addr = this match
    case addr: Addr if (st(addr).isCompletion) => addr
    case notCompValue =>
      val compAddr = st.allocRecord(Some("CompletionRecord"))
      val fields =
        List("Type" -> ty, "Value" -> notCompValue, "Target" -> ENUM_EMPTY)
      for ((k, v) <- fields) st.update(compAddr, Str(k), v)
      compAddr

  /** convert value to pure value see:
    * https://github.com/es-meta/esmeta/issues/66
    */
  def toPureValue(using st: State): PureValue = this match
    case comp: Comp => throw UncheckedAbruptComp(comp)
    case addr: Addr if (st(addr).isCompletion) => throw UncheckedAbrupt(addr)
    case v: PureValue                          => v

  /** type conversion */
  def asStr: String = this match
    case Str(s)      => s
    case CodeUnit(c) => c.toString
    case _           => throw NotStringType(this)
  def asInt: Int = this match
    case Number(n) if n.isValidInt => n.toInt
    case Math(n) if n.isValidInt   => n.toInt
    case _                         => throw NotIntType(this)
  def asAst: Ast = this match
    case AstValue(ast) => ast
    case v             => throw NotAstType(this)
  def asMath: BigDecimal = this match
    case Math(n) => n
    case v       => throw NotDecimalType(this)
  def asEnum: Enum = this match
    case Enum(e) => Enum(e)
    case _       => throw NotEnumType(this)
  def getList(e: Expr, st: State): ListObj = this match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(e, obj)
    case _ => throw NoAddr(e, this)
}

/** completion values */
case class Comp(
  ty: Enum,
  var value: PureValue, // XXX YieldExpression[2,0].Evaluation
  target: Option[String],
) extends Value {
  def targetValue: PureValue = target.fold[PureValue](ENUM_EMPTY)(Str(_))
}
// case class Comp private (
//   ty: Enum,
//   var value: PureValue, // XXX YieldExpression[2,0].Evaluation
//   target: Option[String],
// ) extends Value {
//   def targetValue: PureValue = target.fold[PureValue](ENUM_EMPTY)(Str(_))
// }
// object Comp {
//   def apply(
//     ty: Enum,
//     value: PureValue, // XXX YieldExpression[2,0].Evaluation
//     target: Option[String],
//   ): Comp = ???

// }

/** pure values (values except completion records) */
sealed trait PureValue extends Value

/** addresses */
sealed trait Addr extends PureValue
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** ordering of addresses */
given Ordering[Addr] = Ordering.by(_ match
  case NamedAddr(name)   => (-1L, name)
  case DynamicAddr(long) => (long, ""),
)

object NormalCompValue {
  // def apply(value: Value)(using st: State): Addr =
  //   val addr = st.allocRecord(Some("CompletionRecord"))
  //   NormalCompObj(ENUM_NORMAL, value.toPureValue, ENUM_EMPTY)
  //   addr

  def unapply(addr: Addr)(using st: State): Option[Value] =
    NormalCompObj.unapply(st(addr))
}

/** function values */
sealed trait FuncValue extends PureValue {
  def func: Func
  def captured: Map[Name, Value]
}

/** closures */
case class Clo(func: Func, captured: Map[Name, Value]) extends FuncValue

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends FuncValue

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends PureValue

/** nonterminals for grammar goal symbols */
case class Nt(name: String, params: List[Boolean]) extends PureValue

/** mathematical values */
case class Math(decimal: BigDecimal) extends PureValue
object Math {
  val zero: Math = Math(0)
  val one: Math = Math(1)
  inline def apply(n: Int): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Long): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Double): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: scala.math.BigInt): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(s: String): Math = Math(BigDecimal(s, UNLIMITED))
  inline def from(s: String, b: Int): Math = apply(scala.math.BigInt(s, b))
  inline def fromBinary(s: String): Math = from(s, 2)
  inline def fromOctal(s: String): Math = from(s, 8)
  inline def fromHex(s: String): Math = from(s, 16)

  extension (m: Math) {
    def +(n: Math): Math = Math(m.decimal + n.decimal)
    def -(n: Math): Math = Math(m.decimal - n.decimal)
    def *(n: Math): Math = Math(m.decimal * n.decimal)
    def /(n: Math): Math = Math(m.decimal / n.decimal)
    def pow(n: Math): Math = Math(m.decimal.pow(n.toInt))
    def unary_- : Math = Math(-m.decimal)
    def toInt: Int = m.decimal.toInt
    def toLong: Long = m.decimal.toLong
    def toDouble: Double = m.decimal.toDouble
    def toBigInt: BigInt = BigInt(m.decimal.toBigInt)
    def toBigDecimal: BigDecimal = m.decimal
  }
}

/** infinity values */
case class Infinity(pos: Boolean) extends PureValue

/** enums */
case class Enum(name: String) extends PureValue

/** code units */
case class CodeUnit(c: Char) extends PureValue

/** simple values
  *
  * Simple values are ECMAScript values except objects and symbols. ECMAScript
  * objects and symbols need to be stored in a heap.
  */
sealed trait SimpleValue extends PureValue

/** numeric values */
sealed trait Numeric extends SimpleValue:
  def toMath: Math = this match
    case Number(double) => Math(double)
    case BigInt(bigInt) => Math(bigInt)
case class Number(double: Double) extends Numeric with DoubleEquals
case class BigInt(bigInt: scala.math.BigInt) extends Numeric

/** non-numeric simple values */
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
case object Absent extends SimpleValue
