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
val ECONST_EMPTY = EConst("empty")
val ECONST_UNRESOLVABLE = EConst("unresolvable")
val ECONST_LEXICAL = EConst("lexical")
val ECONST_INITIALIZED = EConst("initialized")
val ECONST_UNINITIALIZED = EConst("uninitialized")
val ECONST_BASE = EConst("base")
val ECONST_DERIVED = EConst("derived")
val ECONST_STRICT = EConst("strict")
val ECONST_GLOBAL = EConst("global")
val ECONST_UNLINKED = EConst("unlinked")
val ECONST_LINKING = EConst("linking")
val ECONST_LINKED = EConst("linked")
val ECONST_EVALUATING = EConst("evaluating")
val ECONST_EVALUATED = EConst("evaluated")
val ECONST_NUMBER = EConst("Number")
val ECONST_BIGINT = EConst("BigInt")
val ECONST_NORMAL = EConst("normal")
val ECONST_BREAK = EConst("break")
val ECONST_CONTINUE = EConst("continue")
val ECONST_RETURN = EConst("return")
val ECONST_THROW = EConst("throw")
val ECONST_SUSPENDED_START = EConst("suspendedStart")
val ECONST_SUSPENDED_YIELD = EConst("suspendedYield")
val ECONST_EXECUTING = EConst("executing")
val ECONST_AWAITING_RETURN = EConst("awaitingDASHreturn")
val ECONST_COMPLETED = EConst("completed")
val ECONST_PENDING = EConst("pending")
val ECONST_FULFILLED = EConst("fulfilled")
val ECONST_REJECTED = EConst("rejected")
val ECONST_FULFILL = EConst("Fulfill")
val ECONST_REJECT = EConst("Reject")

/** predefined globals identifiers */
val GLOBAL_REALM = Global("REALM")
val GLOBAL_EXECUTION_STACK = Global("EXECUTION_STACK")
val GLOBAL_CONTEXT = Prop(GLOBAL_EXECUTION_STACK, EMathVal(0))
val GLOBAL_SYMBOL = Global("SYMBOL")
val GLOBAL_UNDEF_TYPE = Global("Undefined")
val GLOBAL_NULL_TYPE = Global("Null")
val GLOBAL_BOOL_TYPE = Global("Boolean")
val GLOBAL_STRING_TYPE = Global("String")
val GLOBAL_SYMBOL_TYPE = Global("Symbol")
val GLOBAL_NUMBER_TYPE = Global("Number")
val GLOBAL_BIGINT_TYPE = Global("BigInt")
val GLOBAL_OBJECT_TYPE = Global("Object")

/** predefined globals identifier expressions */
val EGLOBAL_EXECUTION_STACK = ERef(GLOBAL_EXECUTION_STACK)
val EGLOBAL_CONTEXT = ERef(GLOBAL_CONTEXT)
val EGLOBAL_SYMBOL = ERef(GLOBAL_SYMBOL)
val EGLOBAL_UNDEF_TYPE = ERef(GLOBAL_UNDEF_TYPE)
val EGLOBAL_NULL_TYPE = ERef(GLOBAL_NULL_TYPE)
val EGLOBAL_BOOL_TYPE = ERef(GLOBAL_BOOL_TYPE)
val EGLOBAL_STRING_TYPE = ERef(GLOBAL_STRING_TYPE)
val EGLOBAL_SYMBOL_TYPE = ERef(GLOBAL_SYMBOL_TYPE)
val EGLOBAL_NUMBER_TYPE = ERef(GLOBAL_NUMBER_TYPE)
val EGLOBAL_BIGINT_TYPE = ERef(GLOBAL_BIGINT_TYPE)
val EGLOBAL_OBJECT_TYPE = ERef(GLOBAL_OBJECT_TYPE)

/** predefined local variables */
val NAME_THIS = Name("this")
val NAME_ARGS_LIST = Name("argumentsList")
val NAME_NEW_TARGET = Name("NewTarget")

/** predefined globals identifier expressions */
val ENAME_THIS = ERef(NAME_THIS)
val ENAME_ARGS_LIST = ERef(NAME_ARGS_LIST)
val ENAME_NEW_TARGET = ERef(NAME_NEW_TARGET)

/** predefined types */
val TYPE_COMPLETION = Type("Completion Record")

/** predefined parameters */
val PARAM_THIS = Func.Param(NAME_THIS)
val PARAM_ARGS_LIST = Func.Param(NAME_ARGS_LIST)
val PARAM_NEW_TARGET = Func.Param(NAME_NEW_TARGET)
