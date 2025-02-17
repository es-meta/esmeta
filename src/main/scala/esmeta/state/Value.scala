package esmeta.state

import esmeta.cfg.{CFG, Func}
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.util.DoubleEquals
import java.math.MathContext.UNLIMITED
import scala.collection.mutable.{Map => MMap}

/** IR values */
sealed trait Value extends StateElem {

  /** check if the value is an expected type */
  def asStr: String = this match
    case Str(s) => s
    case _      => throw NoString(this)
  def asBool: Boolean = this match
    case Bool(b) => b
    case _       => throw NoBoolean(this)
  def asInt: Int = this match
    case Math(n) if n.isValidInt => n.toInt
    case _                       => throw NoInteger(this)
  def asAst: Ast = this match
    case AstValue(ast) => ast
    case v             => throw NoAst(this)
  def asMath: BigDecimal = this match
    case Math(n) => n
    case v       => throw NoMath(this)
  def asCallable: Callable = this match
    case func: Callable => func
    case v              => throw NoCallable(v)
  def asGrammarSymbol: GrammarSymbol = this match
    case g: GrammarSymbol => g
    case v                => throw NoGrammarSymbol(v)
  def asAddr: Addr = this match
    case addr: Addr => addr
    case v          => throw NoAddr(this)
  def asList(st: State): ListObj = this match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(obj)
    case _ => throw NoAddr(this)
}

/** addresses */
sealed trait Addr extends Value
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** ordering of addresses */
given Ordering[Addr] = Ordering.by(_ match
  case NamedAddr(name)   => (-1L, name)
  case DynamicAddr(long) => (long, ""),
)

/** function values */
sealed trait Callable extends Value {
  def func: Func
  def captured: Map[Name, Value]
}

/** closures */
case class Clo(func: Func, captured: Map[Name, Value]) extends Callable

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends Callable

/** primitive values */
sealed trait PrimValue extends Value

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends PrimValue

/** grammar symbols */
case class GrammarSymbol(name: String, params: List[Boolean]) extends PrimValue

/** mathematical values */
case class Math(decimal: BigDecimal) extends PrimValue
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
    def <(n: Math): Boolean = m.decimal < n.decimal
    def >(n: Math): Boolean = m.decimal > n.decimal
    def pow(n: Math): Math = Math(m.decimal.pow(n.toInt))
    def unary_- : Math = Math(-m.decimal)
    def unary_~ : Math = Math(~(m.toInt))
    def toInt: Int = m.decimal.toInt
    def toLong: Long = m.decimal.toLong
    def toDouble: Double = m.decimal.toDouble
    def toBigInt: BigInt = BigInt(m.decimal.toBigInt)
    def toBigDecimal: BigDecimal = m.decimal
  }

  given Ordering[Math] = Ordering.by(_.decimal)
}

/** infinity values */
case class Infinity(pos: Boolean) extends PrimValue

/** enums */
case class Enum(name: String) extends PrimValue

/** code units */
case class CodeUnit(c: Char) extends PrimValue

/** simple values
  *
  * Simple values are ECMAScript values except objects and symbols. ECMAScript
  * objects and symbols need to be stored in a heap.
  */
sealed trait SimpleValue extends PrimValue

/** numeric values */
sealed trait Numeric extends SimpleValue:
  def toMath: Math = this match
    case Number(double) => Math(double)
    case BigInt(bigInt) => Math(bigInt)
case class Number(double: Double) extends Numeric with DoubleEquals {
  def isNaN: Boolean = double.isNaN
}
case class BigInt(bigInt: scala.math.BigInt) extends Numeric

/** non-numeric simple values */
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
