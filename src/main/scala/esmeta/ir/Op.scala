package esmeta.ir

import esmeta.ir.util.Parser

// IR operators
sealed trait Op extends IRElem

// unary operators
enum UOp extends Op:
  // mathematic values
  case Floor
  // numeric values
  case Neg
  // boolean
  case Not
  // bitwise
  case BNot
object UOp extends Parser.From(Parser.uop)

// binary operators
enum BOp extends Op:
  // equality (e.g. is, are)
  case Eq
  // numeric values
  case Add, Sub, Mul, Pow, Div, UMod, Mod, Lt, Equal
  // bitwise
  case BAnd, BOr, BXOr
  // shift
  case LShift, SRShift, URShift
  // boolean
  case And, Or, Xor
object BOp extends Parser.From(Parser.bop)

// variadic operators
enum VOp extends Op:
  // mathematic values
  case Min, Max
  // string
  case Concat
object VOp extends Parser.From(Parser.vop)

// mathematical operators
enum MOp extends Op:
  case Expm1, Log10, Log2, Cos, Cbrt, Exp, Cosh, Sinh, Tanh, Acos, Acosh
  case Asinh, Atanh, Asin, Atan2, Atan, Log1p, Log, Sin, Sqrt, Tan
object MOp extends Parser.From(Parser.mop)

// conversion operators
enum COp extends Op:
  case ToApproxNumber, ToNumber, ToBigInt, ToMath
  case ToStr(radix: Option[Expr])
object COp extends Parser.From(Parser.cop)
