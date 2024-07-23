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

/** predefined enum expressions */
def EENUM_EMPTY = EEnum("empty")
def EENUM_UNRESOLVABLE = EEnum("unresolvable")
def EENUM_LEXICAL = EEnum("lexical")
def EENUM_INITIALIZED = EEnum("initialized")
def EENUM_UNINITIALIZED = EEnum("uninitialized")
def EENUM_BASE = EEnum("base")
def EENUM_DERIVED = EEnum("derived")
def EENUM_STRICT = EEnum("strict")
def EENUM_GLOBAL = EEnum("global")
def EENUM_UNLINKED = EEnum("unlinked")
def EENUM_LINKING = EEnum("linking")
def EENUM_LINKED = EEnum("linked")
def EENUM_EVALUATING = EEnum("evaluating")
def EENUM_EVALUATED = EEnum("evaluated")
def EENUM_NUMBER = EEnum("Number")
def EENUM_BIGINT = EEnum("BigInt")
def EENUM_NORMAL = EEnum("normal")
def EENUM_BREAK = EEnum("break")
def EENUM_CONTINUE = EEnum("continue")
def EENUM_RETURN = EEnum("return")
def EENUM_THROW = EEnum("throw")
def EENUM_SUSPENDED_START = EEnum("suspendedStart")
def EENUM_SUSPENDED_YIELD = EEnum("suspendedYield")
def EENUM_EXECUTING = EEnum("executing")
def EENUM_AWAITING_RETURN = EEnum("awaitingDASHreturn")
def EENUM_COMPLETED = EEnum("completed")
def EENUM_PENDING = EEnum("pending")
def EENUM_FULFILLED = EEnum("fulfilled")
def EENUM_REJECTED = EEnum("rejected")
def EENUM_FULFILL = EEnum("Fulfill")
def EENUM_REJECT = EEnum("Reject")

/** predefined globals identifiers */
def GLOBAL_REALM = Global("REALM")
def GLOBAL_EXECUTION_STACK = Global("EXECUTION_STACK")
def GLOBAL_INTRINSICS = Global("INTRINSICS")
def GLOBAL_CONTEXT = Field(GLOBAL_EXECUTION_STACK, EMath(0))
def GLOBAL_SYMBOL = Global("SYMBOL")
def GLOBAL_MATH_PI = Global("MATH_PI")
def GLOBAL_AGENT_RECORD = Global("AGENT_RECORD")
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

/** predefined auxiliary functions */
inline def getAux(name: String): EClo = EClo("__" + name + "__", Nil)
def AUX_CLAMP = getAux("CLAMP")
def AUX_IS_ARRAY_INDEX = getAux("IS_ARRAY_INDEX")
def AUX_REMOVE_ELEM = getAux("REMOVE_ELEM")
