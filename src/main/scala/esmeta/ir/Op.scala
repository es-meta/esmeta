package esmeta.ir

import esmeta.ir.util.Parser

// IR operators
sealed trait Op extends IRElem

// unary operators
enum UOp extends Op:
  // mathematic values
  case Abs, Floor
  // numeric values
  case Neg
  // boolean
  case Not
  // bitwise
  case BNot
object UOp extends Parser.From[UOp]

// binary operators
enum BOp extends Op:
  // equality (e.g. is, are)
  case Eq
  // numeric values
  case Plus, Sub, Mul, Pow, Div, UMod, Mod, Lt, Equal
  // bitwise
  case BAnd, BOr, BXOr
  // shift
  case LShift, SRShift, URShift
  // boolean
  case And, Or, Xor
object BOp extends Parser.From[BOp]

// variadic operators
enum VOp extends Op:
  // mathematic values
  case Min, Max
object VOp extends Parser.From[VOp]

// conversion operators
enum COp extends Op:
  case ToBigInt, ToNumber, ToMath
  case ToStr(radix: Option[Expr])
object COp extends Parser.From[COp]
