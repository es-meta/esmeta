package esmeta.ir

import esmeta.util.Useful._
import Value._

/** stringify */
val getStringifier = {
  cached[(Boolean), Stringifier](key => {
    val (detail) = key
    new Stringifier(detail)
  })
}

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
