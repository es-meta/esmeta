package esmeta.interp

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import scala.collection.mutable.{Map => MMap}
import esmeta.util.DoubleEquals

/** IR values */
sealed trait Value extends InterpElem {

  /** check abrupt completion */
  def isCompletion: Boolean = this match
    case comp: Comp => true
    case _          => false

  /** check abrupt completion */
  def isAbruptCompletion: Boolean = this match
    case comp: Comp => comp.ty != CONST_NORMAL
    case _          => false

  /** wrap completion */
  def wrapCompletion: Comp = wrapCompletion(CONST_NORMAL)
  def wrapCompletion(ty: Const): Comp = this match
    case comp: Comp      => comp
    case pure: PureValue => Comp(ty, pure, None)

  /** convert value to pure value see:
    * https://github.com/es-meta/esmeta/issues/66
    */
  def toPureValue: PureValue = this match
    case comp: Comp      => throw UncheckedAbrupt(comp)
    case pure: PureValue => pure

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
  def getList(e: Expr, st: State): ListObj = this match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(e, obj)
    case _ => throw NoAddr(e, this)
}

/** completion values */
case class Comp(
  ty: Const,
  var value: PureValue, // XXX YieldExpression[2,0].Evaluation
  target: Option[String],
) extends Value {
  def targetValue: PureValue = target.fold[PureValue](CONST_EMPTY)(Str(_))
}

/** normal completion */
object NormalComp {
  def apply(value: PureValue): Comp =
    Comp(CONST_NORMAL, value, None)
  def unapply(comp: Comp): Option[PureValue] = comp match {
    case Comp(CONST_NORMAL, value, None) => Some(value)
    case _                               => None
  }
}

/** pure values (not completion values) */
sealed trait PureValue extends Value

/** addresses */
sealed trait Addr extends PureValue
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** closures */
case class Clo(func: Func, captured: Map[Name, Value]) extends PureValue

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends PureValue

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends PureValue

/** grammars */
case class Grammar(name: String, params: List[Boolean]) extends PureValue

/** math values */
case class Math(n: BigDecimal) extends PureValue

/** constants */
case class Const(name: String) extends PureValue

/** code units */
case class CodeUnit(c: Char) extends PureValue

/** simple js values */
sealed trait SimpleValue extends PureValue
sealed trait Numeric extends SimpleValue
case class Number(n: Double) extends Numeric with DoubleEquals(n)
case class BigInt(n: scala.math.BigInt) extends Numeric
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
case object Absent extends SimpleValue
