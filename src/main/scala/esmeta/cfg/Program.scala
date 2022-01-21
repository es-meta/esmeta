package esmeta.cfg

import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals
import scala.annotation.tailrec

// -----------------------------------------------------------------------------
// Programs
// -----------------------------------------------------------------------------
case class Program(insts: Array[Inst]) extends CFGElem
object Program extends Parser.From[Program]

// -----------------------------------------------------------------------------
// Labels
// -----------------------------------------------------------------------------
type Label = Int

// -----------------------------------------------------------------------------
// TODO Instruction
// -----------------------------------------------------------------------------
sealed trait Inst extends CFGElem
object Inst extends Parser.From[Inst]

// -----------------------------------------------------------------------------
// TODO Expressions
// -----------------------------------------------------------------------------
sealed trait Expr extends CFGElem
object Expr extends Parser.From[Expr]

// -----------------------------------------------------------------------------
// TODO References
// -----------------------------------------------------------------------------
sealed trait Ref extends CFGElem
object Ref extends Parser.From[Ref]

// -----------------------------------------------------------------------------
// Operators
// -----------------------------------------------------------------------------
enum UOp extends CFGElem:
  case Neg, Not, BNot
object UOp extends Parser.From[UOp]

/** Binary Operators */
enum BOp extends CFGElem:
  case Plus, Sub, Mul, Pow, Div, UMod, Mod, Lt, Eq, Equal, And, Or, Xor, BAnd,
  BOr, BXOr, LShift, SRShift, URShift
object BOp extends Parser.From[BOp]
