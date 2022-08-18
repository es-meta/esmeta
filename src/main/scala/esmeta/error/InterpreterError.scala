package esmeta.error

import esmeta.interpreter.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.cfg.*
import esmeta.state.*

// TODO change to abstract class after refactoring of error in interp
sealed class InterpreterError(msg: String)
  extends ESMetaError(msg, "InterpreterError")

// error with CFG components
case class InterpreterErrorAt(error: InterpreterError, cursor: Cursor)
  extends InterpreterError(s"${error.errMsg} @ $cursor")

// invalid cases
case class InvalidNodeId(nid: Int)
  extends InterpreterError(s"invalid node id: $nid")
case class InvalidCompProp(comp: Comp, prop: PureValue)
  extends InterpreterError(s"invalid completion property: $prop for $comp")
case class InvalidObjProp(obj: Obj, prop: PureValue)
  extends InterpreterError(s"invalid object property: $prop for $obj")
case class InvalidAstProp(ast: Ast, prop: PureValue)
  extends InterpreterError(s"invalid ast property: $prop for ${ast.name}")
case class InvalidRefBase(v: Value)
  extends InterpreterError(s"not a proper reference base: $v")
case class InvalidConversion(cop: COp, expr: Expr, v: Value)
  extends InterpreterError(s"invalid conversion to $cop: $expr, $v")
case class InvalidArgs(name: String, as: List[Value])
  extends InterpreterError(s"invalid arguments: ${as.mkString(", ")} @ $name")
case class InvalidParseSource(expr: Expr, v: Value)
  extends InterpreterError(s"not a proper source for parse: $expr -> $v")
case class InvalidTypeExpr(expr: Expr, v: Value)
  extends InterpreterError(s"not a proper type expression: $expr -> $v")
case class InvalidExit(value: Value)
  extends InterpreterError(s"return not undefined: $value")
case class InvalidASTChildren(a: Ast)
  extends InterpreterError(s"no children for lexical node: $a")

// invalid operands for an operator
case class InvalidUnaryOp(uop: UOp, v: Value)
  extends InterpreterError(s"wrong type for the operator $uop: $v")
case class InvalidBinaryOp(bop: BOp, lv: Value, rv: Value)
  extends InterpreterError(s"wrong type for the operator $bop: $lv and $rv")
case class InvalidVariadicOp(vop: VOp)
  extends InterpreterError(s"no arguments for the operator $vop")

// invalid completion values
sealed abstract class InvalidComp(msg: Option[String])
  extends InterpreterError(s"invalid completion${msg.fold("")(": " + _)}")
case object InvalidComp extends InvalidComp(None)
case class InvalidCompType(v: Value) extends InvalidComp(Some(s"(type) $v"))
case class InvalidCompTarget(v: Value) extends InvalidComp(Some(s"(target) $v"))

// no return values
case object NoReturnValue extends InterpreterError(s"no return value")

// arity mismatches
case class RemainingParams(ps: List[IRFunc.Param])
  extends InterpreterError(s"remaining parameters: ${ps.mkString(", ")}")
case class RemainingArgs(as: List[Value])
  extends InterpreterError(s"remaining arguments: ${as.mkString(", ")}")

// not a specific types
case class NoBoolean(expr: Expr, v: Value)
  extends InterpreterError(s"not a boolean: $expr -> $v")
case class NoString(expr: Expr, v: Value)
  extends InterpreterError(s"not a string: $expr -> $v")
case class NoInteger(expr: Expr, v: Value)
  extends InterpreterError(s"not an integer: $expr -> $v")
case class NoAddr(expr: Expr, v: Value)
  extends InterpreterError(s"not an address: $expr -> $v")
case class NoFunc(expr: Expr, v: Value)
  extends InterpreterError(s"not a function: $expr -> $v")
case class NoAst(expr: Expr, v: Value)
  extends InterpreterError(s"not an abstract syntax tree (AST): $expr -> $v")
case class NoGrammar(expr: Expr, v: Value)
  extends InterpreterError(s"not a grammar symbol: $expr -> $v")
case class NoList(expr: Expr, obj: Obj)
  extends InterpreterError(s"not a list: $expr -> $obj")

// type conversion fails
sealed abstract class InvalidTypeConversion(msg: Option[String])
  extends InterpreterError(s"invalid type conversion${msg.fold("")(": " + _)}")
case class NotStringType(v: Value)
  extends InvalidTypeConversion(Some(s"$v is not string"))
case class NotAstType(v: Value)
  extends InvalidTypeConversion(Some(s"$v is not ast"))
case class NotIntType(v: Value)
  extends InvalidTypeConversion(Some(s"$v is not integer"))
case class NotDecimalType(v: Value)
  extends InvalidTypeConversion(Some(s"$v is not decimal number"))

// undefined values
case class UnknownId(x: Id) extends InterpreterError(s"unknown variable: $x")
case class UnknownAddr(addr: Addr)
  extends InterpreterError(s"unknown address: $addr")
case class UnknownFunc(name: String)
  extends InterpreterError(s"unknown function: $name")
case class WrongStringRef(str: String, prop: PureValue)
  extends InterpreterError(s"wrong access of string reference: $str.$prop")

// missing cases
case class UncheckedAbrupt(comp: Comp)
  extends InterpreterError(s"unchecked abrupt completion: $comp")

// assertion failed
case class AssertionFail(expr: Expr)
  extends InterpreterError(s"assertion failure: $expr")
case class OutOfRange(list: ListObj, k: Int)
  extends InterpreterError(s"out of range: $k of $list")
