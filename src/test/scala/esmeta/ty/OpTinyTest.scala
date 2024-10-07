package esmeta.ty

/** operation test */
class OpTinyTest extends TyTest {
  val name: String = "tyOpTest"

  // registration
  def init: Unit = {
    checkEqual("or")(
      (ObjectT || FunctionT) -> ObjectT,
      (ReturnT || AbruptT("break")) -> (ReturnT || BreakT),
      (FunctionT || RecordT("ECMAScriptFunctionObject")) -> FunctionT,
    )

    checkEqual("and")(
      (ObjectT && FunctionT) -> FunctionT,
      (AbruptT && ReturnT) -> ReturnT,
      (ConstructorT && RecordT("ECMAScriptFunctionObject")) ->
      RecordT("ECMAScriptFunctionObject"),
    )

    checkEqual("order")(
      (ReturnT <= AbruptT("break")) -> false,
    )
  }

  init
}
