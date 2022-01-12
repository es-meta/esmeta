package esmeta.ir

import esmeta.util.BaseUtils._
import Value._

/** predefined constants */
val CONST_EMPTY = Const("empty")
val CONST_UNRESOLVABLE = Const("unresolvable")
val CONST_LEXICAL = Const("lexical")
val CONST_INITIALIZED = Const("initialized")
val CONST_UNINITIALIZED = Const("uninitialized")
val CONST_BASE = Const("base")
val CONST_DERIVED = Const("derived")
val CONST_STRICT = Const("strict")
val CONST_GLOBAL = Const("global")
val CONST_UNLINKED = Const("unlinked")
val CONST_LINKING = Const("linking")
val CONST_LINKED = Const("linked")
val CONST_EVALUATING = Const("evaluating")
val CONST_EVALUATED = Const("evaluated")
val CONST_NUMBER = Const("Number")
val CONST_BIGINT = Const("BigInt")
val CONST_NORMAL = Const("normal")
val CONST_BREAK = Const("break")
val CONST_CONTINUE = Const("continue")
val CONST_RETURN = Const("return")
val CONST_THROW = Const("throw")
val CONST_SUSPENDED_START = Const("suspendedStart")
val CONST_SUSPENDED_YIELD = Const("suspendedYield")
val CONST_EXECUTING = Const("executing")
val CONST_AWAITING_RETURN = Const("awaitingDASHreturn")
val CONST_COMPLETED = Const("completed")
val CONST_PENDING = Const("pending")
val CONST_FULFILLED = Const("fulfilled")
val CONST_REJECTED = Const("rejected")
val CONST_FULFILL = Const("Fulfill")
val CONST_REJECT = Const("Reject")

/** predefined identifiers */
val ID_RETURN = Id("RETURN")

/** predefined string */
val STR_TOP_LEVEL = "TOP_LEVEL"

//TODO need to be moved
// The below functions were in $JISET_SRC/ir/package.scala

/** equality between doubles */
def doubleEquals(left: Double, right: Double): Boolean = {
  if (left.isNaN && right.isNaN) true
  else if (isNegZero(left) && !isNegZero(right)) false
  else if (!isNegZero(left) && isNegZero(right)) false
  else left == right
}

/** modulo operation */
def modulo(l: Double, r: Double): Double = {
  l % r
}

/** unsigned modulo operation */
def unsigned_modulo(l: Double, r: Double): Double = {
  val m = l % r
  if (m * r < 0.0) m + r
  else m
}

/** modulo operation for bigints */
def modulo(l: BigInt, r: BigInt): BigInt = {
  l % r
}

/** unsigned modulo operation for bigints */
def unsigned_modulo(l: BigInt, r: BigInt): BigInt = {
  val m = l % r
  if (m * r < 0) m + r
  else m
}

/** get proper number value */
def number(x: BigDecimal): Value = {
  if (x.toLong == x) INum(x.toLong)
  else Num(x.toDouble)
}

/** negative zero check */
def isNegZero(double: Double): Boolean = (1 / double).isNegInfinity
