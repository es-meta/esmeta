package esmeta.cfg

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
val GLOBAL_CONTEXT = Global("CONTEXT")
val GLOBAL_EXECUTION_STACK = Global("EXECUTION_STACK")
val GLOBAL_REALM = Global("REALM")

/** predefined globals identifier expressions */
val EGLOBAL_CONTEXT = ERef(GLOBAL_CONTEXT)
val EGLOBAL_EXECUTION_STACK = ERef(GLOBAL_EXECUTION_STACK)
val EGLOBAL_REALM = ERef(GLOBAL_REALM)

/** predefined local variables */
val NAME_THIS = Name("this")
val NAME_ARGS_LIST = Name("argumentsList")
val NAME_NEW_TARGET = Name("NewTarget")

/** predefined globals identifier expressions */
val ENAME_THIS = ERef(NAME_THIS)
val ENAME_ARGS_LIST = ERef(NAME_ARGS_LIST)
val ENAME_NEW_TARGET = ERef(NAME_NEW_TARGET)

/** predefined parameters */
val PARAM_THIS = Param(NAME_THIS)
val PARAM_ARGS_LIST = Param(NAME_ARGS_LIST)
val PARAM_NEW_TARGET = Param(NAME_NEW_TARGET)
