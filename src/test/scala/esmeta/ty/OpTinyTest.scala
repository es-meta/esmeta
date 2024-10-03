package esmeta.ty

/** operation test */
class OpTinyTest extends TyTest {
  val name: String = "tyOpTest"

  // registration
  def init: Unit = {
    checkEqual("or")(
      (ObjectT || FunctionT) -> ObjectT,
      (ReturnT || AbruptT("break")) -> AbruptT("break", "return"),
      (FunctionT || RecordT("ECMAScriptFunctionObject")) -> FunctionT,
    )

    checkEqual("and")(
      (ObjectT && FunctionT) -> FunctionT,
      (AbruptT && ReturnT) -> ReturnT,
    )

    checkEqual("order")(
      (ReturnT <= AbruptT("break")) -> false,
    )
  }

  init
}
