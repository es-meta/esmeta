package esmeta.error

import esmeta.interpreter.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ty.*

// TODO change to abstract class after refactoring of error in interp
sealed class InterpreterError(msg: String)
  extends ESMetaError(msg, "InterpreterError")

// error with CFG components
case class InterpreterErrorAt(error: InterpreterError, cursor: Cursor)
  extends InterpreterError(s"${error.errMsg} @ $cursor")

// invalid cases
case class InvalidNodeId(nid: Int)
  extends InterpreterError(s"invalid node id: $nid")
case class InvalidCompField(value: Value, field: Value)
  extends InterpreterError(s"invalid completion field: $field for $value")
case class InvalidObjField(obj: Obj, field: Value)
  extends InterpreterError(s"invalid object field: $field for $obj")
case class InvalidObjOp(obj: Obj, op: String)
  extends InterpreterError(s"invalid object operation: $op for $obj")
case class InvalidAstField(ast: Ast, field: Value)
  extends InterpreterError(s"invalid ast field: $field for ${ast.name}")
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
case class InvalidASTChildren(ast: Ast)
  extends InterpreterError(s"no children for lexical node: |${ast.name}|")
case class InvalidASTItem(ast: Ast, name: String)
  extends InterpreterError(s"invalid item: |${ast.name}| (expected: |$name|)")
case class InvalidTypedValue(value: Value, ty: Ty)
  extends InterpreterError(s"unexpected typed value: $value (expected: $ty)")
case class InvalidSizeOf(x: Value | Obj)
  extends InterpreterError(s"invalid size of: $x")

// invalid operands for an operator
case class InvalidUnaryOp(uop: UOp, v: Value)
  extends InterpreterError(s"wrong type for the operator $uop: $v")
case class InvalidBinaryOp(bop: BOp, lv: Value, rv: Value)
  extends InterpreterError(s"wrong type for the operator $bop: $lv and $rv")
case class InvalidVariadicOp(vop: VOp)
  extends InterpreterError(s"no arguments for the operator $vop")
case class InvalidMathOp(mop: MOp, vs: List[Value])
  extends InterpreterError(
    s"invalid mathematical operation: $mop with ${vs.mkString(", ")}",
  )

// invalid completion values
sealed abstract class InvalidComp(msg: Option[String])
  extends InterpreterError(s"invalid completion${msg.fold("")(": " + _)}")
case object InvalidComp extends InvalidComp(None)
case class InvalidCompType(v: Value) extends InvalidComp(Some(s"(type) $v"))
case class InvalidCompTarget(v: Value) extends InvalidComp(Some(s"(target) $v"))

// no return values
case object NoReturnValue extends InterpreterError(s"no return value")

// arity mismatches
case class RemainingParams(ps: List[Param])
  extends InterpreterError(s"remaining parameters: ${ps.mkString(", ")}")
case class RemainingArgs(as: List[Value])
  extends InterpreterError(s"remaining arguments: ${as.mkString(", ")}")

// unexpected values
class Unexpected(expected: String, value: String)
  extends InterpreterError(s"$expected expected but got: $value")

// unexpected values
class UnexpectedValue(expected: String, value: Value)
  extends Unexpected(expected, value.toString)
case class NoBoolean(v: Value) extends UnexpectedValue("boolean", v)
case class NoString(v: Value) extends UnexpectedValue("string", v)
case class NoInteger(v: Value) extends UnexpectedValue("integer", v)
case class NoMath(v: Value) extends UnexpectedValue("mathematical value", v)
case class NoAddr(v: Value) extends UnexpectedValue("address", v)
case class NoCallable(v: Value) extends UnexpectedValue("callable", v)
case class NoAst(v: Value) extends UnexpectedValue("ast", v)
case class NoGrammarSymbol(v: Value)
  extends UnexpectedValue("grammar symbol", v)

// unexpected objects
class UnexpectedObj(expected: String, obj: Obj)
  extends Unexpected(expected, obj.toString)
case class NoRecord(obj: Obj) extends UnexpectedObj("record", obj)
case class NoMap(obj: Obj) extends UnexpectedObj("map", obj)
case class NoList(obj: Obj) extends UnexpectedObj("list", obj)

// undefined values
case class UnknownVar(x: Var) extends InterpreterError(s"unknown variable: $x")
case class UnknownAddr(addr: Addr)
  extends InterpreterError(s"unknown address: $addr")
case class UnknownFunc(name: String)
  extends InterpreterError(s"unknown function: $name")
case class WrongStringRef(str: String, field: Value)
  extends InterpreterError(s"wrong access of string reference: $str.$field")

// missing cases
case object UncheckedUnint extends InterpreterError(s"unchecked uninit")
case class UncheckedAbrupt(value: Value) // TODO remove
  extends InterpreterError(s"unchecked abrupt completion: $value")

// assertion failed
case class AssertionFail(expr: Expr)
  extends InterpreterError(s"assertion failure: $expr")
case class OutOfRange(list: ListObj, k: Int)
  extends InterpreterError(s"out of range: $k of $list")

case class BranchNotYetSupported()
  extends InterpreterError("Partial Interpreter can't handle branch for now")
