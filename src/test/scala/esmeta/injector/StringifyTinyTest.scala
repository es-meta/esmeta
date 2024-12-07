package esmeta.injector

/** stringify test */
class StringifyTinyTest extends InjectorTest {
  val name: String = "injectorStringifyTest"

  // registration
  def init: Unit = {
    checkStringify("Assertion")(
      hasValue ->
      "$assert.sameValue(x.y.z, null);",
      isExtensible ->
      "$assert.sameValue(Object.isExtensible(x.y.z), true);",
      isNotExtensible ->
      "$assert.sameValue(Object.isExtensible(x.y.z), false);",
      isCallable ->
      "$assert.callable(x.y.z);",
      isNotCallable ->
      "$assert.notCallable(x.y.z);",
      isConstructable ->
      "$assert.constructable(x.y.z);",
      isNotConstructable ->
      "$assert.notConstructable(x.y.z);",
      compareArray ->
      "$assert.compareArray($Reflect.ownKeys(x.y.z), [1, 2, 3], x.y.z);",
      sameObject ->
      "$assert.sameValue(x.y.z, a.b.c);",
      verifyProperty -> """$verifyProperty(x.y.z, p, {
      |  a: null,
      |});""".stripMargin,
    )

    checkStringify("ExitTag")(
      normal -> "normal",
      timeout -> "timeout",
      specError -> "spec-error: Func[0]",
      throwValue -> "throw: null",
    )

    checkStringify("ConformTest")(
      conformTest -> """// [EXIT] normal
      |var x = 42;
      |// Assertions
      |$assert.sameValue(x.y.z, null);
      |$assert.callable(x.y.z);""".stripMargin,
    )
  }

  init
}
