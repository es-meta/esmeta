package esmeta.ir

import esmeta.ir.util.*
import esmeta.util.BaseUtils.*

/** IR elements */
trait IRElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String =
    val stringifier = IRElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
}
object IRElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { Stringifier(_, _) }
}

/** predefined constant expressions */
def ECONST_EMPTY = EConst("empty")
def ECONST_UNRESOLVABLE = EConst("unresolvable")
def ECONST_LEXICAL = EConst("lexical")
def ECONST_INITIALIZED = EConst("initialized")
def ECONST_UNINITIALIZED = EConst("uninitialized")
def ECONST_BASE = EConst("base")
def ECONST_DERIVED = EConst("derived")
def ECONST_STRICT = EConst("strict")
def ECONST_GLOBAL = EConst("global")
def ECONST_UNLINKED = EConst("unlinked")
def ECONST_LINKING = EConst("linking")
def ECONST_LINKED = EConst("linked")
def ECONST_EVALUATING = EConst("evaluating")
def ECONST_EVALUATED = EConst("evaluated")
def ECONST_NUMBER = EConst("Number")
def ECONST_BIGINT = EConst("BigInt")
def ECONST_NORMAL = EConst("normal")
def ECONST_BREAK = EConst("break")
def ECONST_CONTINUE = EConst("continue")
def ECONST_RETURN = EConst("return")
def ECONST_THROW = EConst("throw")
def ECONST_SUSPENDED_START = EConst("suspendedStart")
def ECONST_SUSPENDED_YIELD = EConst("suspendedYield")
def ECONST_EXECUTING = EConst("executing")
def ECONST_AWAITING_RETURN = EConst("awaitingDASHreturn")
def ECONST_COMPLETED = EConst("completed")
def ECONST_PENDING = EConst("pending")
def ECONST_FULFILLED = EConst("fulfilled")
def ECONST_REJECTED = EConst("rejected")
def ECONST_FULFILL = EConst("Fulfill")
def ECONST_REJECT = EConst("Reject")

/** predefined globals identifiers */
def GLOBAL_REALM = Global("REALM")
def GLOBAL_EXECUTION_STACK = Global("EXECUTION_STACK")
def GLOBAL_INTRINSICS = Global("INTRINSICS")
def GLOBAL_CONTEXT = Prop(GLOBAL_EXECUTION_STACK, EMath(0))
def GLOBAL_SYMBOL = Global("SYMBOL")
def GLOBAL_MATH_PI = Global("MATH_PI")
def GLOBAL_UNDEF_TYPE = Global("Undefined")
def GLOBAL_NULL_TYPE = Global("Null")
def GLOBAL_BOOL_TYPE = Global("Boolean")
def GLOBAL_STRING_TYPE = Global("String")
def GLOBAL_SYMBOL_TYPE = Global("Symbol")
def GLOBAL_NUMBER_TYPE = Global("Number")
def GLOBAL_BIGINT_TYPE = Global("BigInt")
def GLOBAL_OBJECT_TYPE = Global("Object")

/** predefined globals identifier expressions */
def EGLOBAL_EXECUTION_STACK = ERef(GLOBAL_EXECUTION_STACK)
def EGLOBAL_INTRINSICS = ERef(GLOBAL_INTRINSICS)
def EGLOBAL_CONTEXT = ERef(GLOBAL_CONTEXT)
def EGLOBAL_SYMBOL = ERef(GLOBAL_SYMBOL)
def EGLOBAL_MATH_PI = ERef(GLOBAL_MATH_PI)
def EGLOBAL_UNDEF_TYPE = ERef(GLOBAL_UNDEF_TYPE)
def EGLOBAL_NULL_TYPE = ERef(GLOBAL_NULL_TYPE)
def EGLOBAL_BOOL_TYPE = ERef(GLOBAL_BOOL_TYPE)
def EGLOBAL_STRING_TYPE = ERef(GLOBAL_STRING_TYPE)
def EGLOBAL_SYMBOL_TYPE = ERef(GLOBAL_SYMBOL_TYPE)
def EGLOBAL_NUMBER_TYPE = ERef(GLOBAL_NUMBER_TYPE)
def EGLOBAL_BIGINT_TYPE = ERef(GLOBAL_BIGINT_TYPE)
def EGLOBAL_OBJECT_TYPE = ERef(GLOBAL_OBJECT_TYPE)

/** predefined local variable names */
def THIS_STR = "this"
def ARGS_LIST_STR = "ArgumentsList"
def NEW_TARGET_STR = "NewTarget"

/** predefined local variables */
def NAME_THIS = Name(THIS_STR)
def NAME_ARGS_LIST = Name(ARGS_LIST_STR)
def NAME_NEW_TARGET = Name(NEW_TARGET_STR)

/** predefined globals identifier expressions */
def ENAME_THIS = ERef(NAME_THIS)
def ENAME_ARGS_LIST = ERef(NAME_ARGS_LIST)
def ENAME_NEW_TARGET = ERef(NAME_NEW_TARGET)

/** predefined parameters */
def PARAM_THIS = Param(NAME_THIS)
def PARAM_ARGS_LIST = Param(NAME_ARGS_LIST)
def PARAM_NEW_TARGET = Param(NAME_NEW_TARGET)
