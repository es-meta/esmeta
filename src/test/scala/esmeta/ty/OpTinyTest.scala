package esmeta.ty

/** operation test */
class OpTinyTest extends TyTest {
  val name: String = "tyOpTest"

  // registration
  def init: Unit = {
    checkEqual("or")(
      (ObjectT || FunctionT) -> ObjectT,
      (ReturnT || AbruptT("break")) -> (ReturnT || BreakT),
      (FunctionT || FunctionT) -> FunctionT,
      (FunctionT || RecordT("ECMAScriptFunctionObject")) -> FunctionT,
    )

    checkEqual("and")(
      (ObjectT && FunctionT) -> FunctionT,
      (AbruptT && ReturnT) -> ReturnT,
      (FunctionT && FunctionT) -> FunctionT,
      (ConstructorT && RecordT("ECMAScriptFunctionObject")) ->
      RecordT("ECMAScriptFunctionObject", List("Construct")),
      (FunctionT && RecordT("ProxyExoticObject")) ->
      RecordT("ProxyExoticObject", List("Call")),
    )

    checkEqual("order")(
      (ReturnT <= AbruptT("break")) -> false,
    )
  }

  init
}
