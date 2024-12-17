package esmeta.injector.util

import esmeta.LINE_SEP
import esmeta.injector.*
import esmeta.injector.Injector.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

/** stringifier for ECMAScript */
class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[InjectorElem] = (app, elem) =>
    elem match
      case elem: ConformTest => testRule(app, elem)
      case elem: ExitTag     => exitTagRule(app, elem)
      case elem: Assertion   => assertRule(app, elem)

  // conformance tests
  given testRule: Rule[ConformTest] = (app, test) =>
    val ConformTest(id, script, exitTag, isAsync, assertions) = test
    val delayHead = "$delay(() => {"
    val delayTail = "});"

    if (detail) app >> "\"use strict\";" >> LINE_SEP
    app >> "// [EXIT] " >> exitTag
    app :> script
    if (exitTag.isNormal) {
      app :> "// Assertions"
      def body = {
        // prepend auxiliary definitions for assertions
        if (detail) app >> header
        // handle async tests by delaying the execution
        if (isAsync) app.wrap(delayHead, delayTail) {
          assertions.foreach(app :> _)
        }
        else assertions.foreach(app :> _)
      }
      if (detail) (app :> "").wrap("(() => {", "})();")(body)
      else body
    }
    app

  // exit tags
  given exitTagRule: Rule[ExitTag] = (app, tag) =>
    import ExitTag.*
    tag match
      case Normal                   => app >> s"normal"
      case Timeout                  => app >> s"timeout"
      case SpecError(error, cursor) => app >> s"spec-error: $cursor"
      case ThrowValue(values)       => app >> s"throw: ${values.mkString(", ")}"

  // assertions
  given assertRule: Rule[Assertion] = (app, assert) =>
    given Rule[SimpleValue] = (app, value) =>
      app >> (
        value match
          case Number(n) => n.toString
          case v         => v.toString
      )

    given Rule[Map[String, SimpleValue]] = (app, desc) =>
      app.wrap(
        desc.map((field, value) => app :> field >> ": " >> value >> ","),
      )

    // TODO(@hyp3rflow): ignore exception thrown during executing injected assertions
    // https://github.com/kaist-plrg/esmeta/commit/a083e94f26bc8b39a4c76d9e9372b0cadb5d827f
    assert match
      case HasValue(x, v) =>
        app >> s"$$assert.sameValue($x, " >> v >> ");"
      case IsExtensible(addr, path, b) =>
        app >> s"$$assert.sameValue(Object.isExtensible($path), $b);"
      case IsCallable(addr, path, b) =>
        app >> s"$$assert.${if b then "c" else "notC"}allable($path);"
      case IsConstructable(addr, path, b) =>
        app >> s"$$assert.${if b then "c" else "notC"}onstructable($path);"
      case CompareArray(addr, path, array) =>
        app >> s"$$assert.compareArray($$Reflect.ownKeys($path), ${array
          .mkString("[", ", ", "]")}, $path);"
      case SameObject(addr, path, origPath) =>
        app >> s"$$assert.sameValue($path, $origPath);"
      case VerifyProperty(addr, path, propStr, desc) =>
        app >> s"$$verifyProperty($path, $propStr, " >> desc >> ");"
}
