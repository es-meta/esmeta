package esmeta.test262.util

import esmeta.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.test262.{*, given}
import io.circe.*, io.circe.syntax.*
import java.io.*

/** Test262 test filter */
case class TestFilter(tests: List[MetaData]) {

  /** configuration summary for applicable tests */
  lazy val summary = targetTests
    .remove(
      "longTest" -> (m => longTest.contains(removedExt(m.relName))),
      "veryLongTest" -> (m => veryLongTest.contains(removedExt(m.relName))),
      "yet" -> (m => yets.contains(removedExt(m.relName))),
    )
    .summary

  /** configuration summary for long tests */
  lazy val longSummary = targetTests
    .remove(
      "non longTest" -> (m => !longTest.contains(removedExt(m.relName))),
    )
    .summary

  /** configuration summary for very long tests */
  lazy val veryLongSummary = targetTests
    .remove(
      "non veryLongTest" -> (m => !veryLongTest.contains(removedExt(m.relName))),
    )
    .summary

  /** target Test262 tests */
  lazy val targetTests = getTests(features = languageFeatures)

  /** a getter of tests for given language features */
  def getTests(features: List[String] = Nil): List[MetaData] = tests.remove(
    "harness" -> (_.relName.startsWith("harness")),
    "internationalisation" -> (_.relName.startsWith("intl")),
    "annex" -> (m =>
      m.relName.startsWith("annex") ||
      m.relName.contains("__proto__"),
    ),
    "in-progress features" -> (m =>
      !m.features.forall(features.contains(_)) ||
      manualInprogress.map(_._1).contains(removedExt(m.relName)),
    ),
    "non-strict" -> (m =>
      m.flags.contains("noStrict") ||
      m.flags.contains("raw") ||
      manualNonstrict.contains(removedExt(m.relName)),
    ),
    "module" -> (m =>
      m.flags.contains("module") ||
      m.relName.startsWith("language/module-code/") ||
      m.relName.startsWith("language/import/") ||
      m.relName.startsWith("language/expressions/dynamic-import/") ||
      m.relName.startsWith("language/expressions/import.meta/"),
    ),
    "early errors" -> (m =>
      !m.negative.isEmpty ||
      manualEarlyError.contains(removedExt(m.relName)),
    ),
    "inessential built-in objects" -> (m =>
      m.flags.contains("CanBlockIsFalse") ||
      m.flags.contains("CanBlockIsTrue") ||
      !m.locales.isEmpty,
    ),
    "non tests" -> (m => manualNonTest.contains(removedExt(m.relName))),
    "wrong tests" -> (m => wrongTest.contains(removedExt(m.relName))),
  )

  /** language features in Test262
    * @url
    *   https://github.com/tc39/test262/blob/main/features.txt
    */
  lazy val languageFeatures = List(
    "ArrayBuffer",
    "Array.prototype.values",
    "arrow-function",
    "async-iteration",
    "async-functions",
    "Atomics",
    "caller",
    "class",
    "computed-property-names",
    "const",
    "cross-realm",
    "DataView",
    "DataView.prototype.getFloat32",
    "DataView.prototype.getFloat64",
    "DataView.prototype.getInt16",
    "DataView.prototype.getInt32",
    "DataView.prototype.getInt8",
    "DataView.prototype.getUint16",
    "DataView.prototype.getUint32",
    "DataView.prototype.setUint8",
    "default-parameters",
    "destructuring-assignment",
    "destructuring-binding",
    "for-of",
    "Float32Array",
    "Float64Array",
    "generators",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "json-superset",
    "let",
    "Map",
    "new.target",
    "object-rest",
    "object-spread",
    "Object.is",
    "optional-catch-binding",
    "Promise",
    "Promise.prototype.finally",
    "Proxy",
    "Reflect",
    "Reflect.construct",
    "Reflect.set",
    "Reflect.setPrototypeOf",
    "regexp-dotall",
    "regexp-lookbehind",
    "regexp-named-groups",
    "regexp-unicode-property-escapes",
    "rest-parameters",
    "Set",
    "SharedArrayBuffer",
    "String.fromCodePoint",
    "String.prototype.endsWith",
    "String.prototype.includes",
    "super",
    "Symbol",
    "Symbol.asyncIterator",
    "Symbol.hasInstance",
    "Symbol.isConcatSpreadable",
    "Symbol.iterator",
    "Symbol.match",
    "Symbol.replace",
    "Symbol.search",
    "Symbol.species",
    "Symbol.split",
    "Symbol.toPrimitive",
    "Symbol.toStringTag",
    "Symbol.unscopables",
    "tail-call-optimization",
    "template",
    "TypedArray",
    "u180e",
    "Uint8Array",
    "Uint16Array",
    "Uint32Array",
    "Uint8ClampedArray",
    "WeakMap",
    "WeakSet",
    // XXX: See appendix B Additional ECMAScript Features for Web Browsers
    // "__proto__",
    // "__getter__",
    // "__setter__",
    // BigInt: https://github.com/tc39/proposal-bigint
    "BigInt",
    // Align detached buffer semantics with web reality
    // https://github.com/tc39/ecma262/pull/2164
    "align-detached-buffer-semantics-with-web-reality",
    // Logical assignment (&&=, ||=, ??=)
    // https://github.com/tc39/proposal-logical-assignment
    "logical-assignment-operators",
    // Enumeration order for for-in
    // https://github.com/tc39/proposal-for-in-order
    "for-in-order",
    // String.prototype.replaceAll
    // https://github.com/tc39/proposal-string-replaceall
    "String.prototype.replaceAll",
    // Promise.any
    // https://github.com/tc39/proposal-promise-any
    "Promise.any",
    "AggregateError",
    // Nullish Coalesce Expression
    // https://github.com/tc39/proposal-nullish-coalescing
    "coalesce-expression",
    // Optional Chaining
    // https://github.com/tc39/proposal-optional-chaining
    "optional-chaining",
    // WeakRef
    // https://github.com/tc39/proposal-weakref
    // "WeakRef",
    // "FinalizationRegistry",
    // import.meta
    // https://github.com/tc39/proposal-import-meta
    "import.meta",
    // # `export * as namespace from module`
    // # https://github.com/tc39/ecma262/pull/1174
    "export-star-as-namespace-from-module",
    // Global
    // https://github.com/tc39/proposal-global
    "globalThis",
    // Well-formed JSON.stringify
    // https://github.com/tc39/proposal-well-formed-stringify
    "well-formed-json-stringify",
    // Symbol.prototype.description
    // https://github.com/tc39/proposal-symbol-description
    "Symbol.prototype.description",
    // String.prototype.matchAll
    // https://github.com/tc39/proposal-string-matchall
    "String.prototype.matchAll",
    "Symbol.matchAll",
    // Numeric Separator Literal
    // https://github.com/tc39/proposal-numeric-separator
    "numeric-separator-literal",
    // String Trimming
    // https://github.com/tc39/proposal-string-left-right-trim
    // Includes all tests for:
    // String.prototype.{trimStart, trimEnd, trimLeft, trimRight}
    "string-trimming",
    "String.prototype.trimEnd",
    "String.prototype.trimStart",
    // Array.prototype.flat and Array.prototype.flatMap
    // https://github.com/tc39/proposal-flatMap
    "Array.prototype.flat",
    "Array.prototype.flatMap",
    // Promise.allSettled
    // https://github.com/tc39/proposal-promise-allSettled
    "Promise.allSettled",
    // Object.fromEntries
    // https://github.com/tc39/proposal-object-from-entries
    "Object.fromEntries",
    // Dynamic Import
    // https://github.com/tc39/proposal-dynamic-import
    "dynamic-import",
    // Missing checks in Proxy internal methods
    // https://github.com/tc39/ecma262/pull/666
    "proxy-missing-checks",
    // Class Fields
    // https://github.com/tc39/proposal-class-fields
    "class-fields-public",
    "class-fields-private",
    // Class Static Fields & Methods
    // https://github.com/tc39/proposal-static-class-features/
    "class-static-fields-public",
    "class-static-fields-private",
    "class-static-methods-private",
    // Class Private methods and getter/setters
    // https://github.com/tc39/proposal-private-methods
    "class-methods-private",
    // Top Level Await
    // https://github.com/tc39/proposal-top-level-await
    "top-level-await",
    // Item Method
    // https://github.com/tc39/proposal-item-method
    "Array.prototype.at",
    "String.prototype.at",
    "TypedArray.prototype.at",
    // Object.hasOwn
    // https://github.com/tc39/proposal-accessible-object-hasownproperty
    "Object.hasOwn",
    // Class static initialization blocks
    // https://github.com/tc39/proposal-class-static-block
    "class-static-block",
    // Ergonomic brand checks for Private Fields
    // https://github.com/tc39/proposal-private-fields-in-in
    "class-fields-private-in",
    // Error cause
    // https://github.com/tc39/proposal-error-cause
    "error-cause",
  )

  /** manually filtered out non-strict mode tests */
  lazy val manualNonstrict = List(
    "language/eval-code/indirect/always-non-strict",
    "language/eval-code/indirect/non-definable-global-function",
    "language/eval-code/indirect/non-definable-global-generator",
    "language/eval-code/indirect/non-definable-global-var",
    "language/eval-code/indirect/var-env-func-init-global-new",
    "language/eval-code/indirect/var-env-func-init-global-update-configurable",
    "language/eval-code/indirect/var-env-func-init-multi",
    "language/eval-code/indirect/var-env-func-non-strict",
    "language/eval-code/indirect/var-env-global-lex-non-strict",
    "language/eval-code/indirect/var-env-var-init-global-exstng",
    "language/eval-code/indirect/var-env-var-init-global-new",
    "language/eval-code/indirect/var-env-var-non-strict",
    "language/statements/variable/12.2.1-21-s",
    "language/function-code/10.4.3-1-13-s",
    "language/expressions/this/S11.1.1_A4.1",
    "language/function-code/10.4.3-1-13-s",
    "language/function-code/10.4.3-1-13gs",
    "language/function-code/10.4.3-1-15-s",
    "language/function-code/10.4.3-1-15gs",
    "built-ins/Function/S15.3.2.1_A3_T8",
    "built-ins/Function/S15.3.5_A2_T1",
    "built-ins/Function/S15.3.5_A2_T2",
    "built-ins/Function/S15.3_A3_T1",
    "built-ins/Function/S15.3_A3_T2",
    "built-ins/Function/S15.3_A3_T5",
    "built-ins/Function/S15.3_A3_T6",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T1",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T2",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T3",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T4",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T5",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T7",
    "built-ins/Function/prototype/apply/S15.3.4.3_A3_T9",
    "built-ins/Function/prototype/apply/S15.3.4.3_A5_T1",
    "built-ins/Function/prototype/apply/S15.3.4.3_A5_T2",
    "built-ins/Function/prototype/apply/S15.3.4.3_A7_T1",
    "built-ins/Function/prototype/apply/S15.3.4.3_A7_T2",
    "built-ins/Function/prototype/apply/S15.3.4.3_A7_T5",
    "built-ins/Function/prototype/apply/S15.3.4.3_A7_T7",
    "built-ins/Function/prototype/apply/S15.3.4.3_A7_T8",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T1",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T2",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T3",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T4",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T5",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T7",
    "built-ins/Function/prototype/call/S15.3.4.4_A3_T9",
    "built-ins/Function/prototype/call/S15.3.4.4_A5_T1",
    "built-ins/Function/prototype/call/S15.3.4.4_A5_T2",
    "built-ins/Function/prototype/call/S15.3.4.4_A6_T1",
    "built-ins/Function/prototype/call/S15.3.4.4_A6_T2",
    "built-ins/Function/prototype/call/S15.3.4.4_A6_T5",
    "built-ins/Function/prototype/call/S15.3.4.4_A6_T7",
    "built-ins/Function/prototype/call/S15.3.4.4_A6_T8",
    "built-ins/Object/entries/tamper-with-global-object",
    "built-ins/Object/values/tamper-with-global-object",
    "language/statements/variable/12.2.1-17-s",
    "language/literals/numeric/7.8.3-3gs",
  )

  /** manually filtered out tests for EarlyErorr */
  lazy val manualEarlyError = List(
    "built-ins/Function/StrictFunction_reservedwords_with",
    "language/arguments-object/10.5-1-s",
    "language/arguments-object/10.5-7-b-1-s",
    "language/eval-code/direct/new.target",
    "language/eval-code/direct/new.target-arrow",
    "language/eval-code/direct/parse-failure-3",
    "language/eval-code/direct/parse-failure-4",
    "language/eval-code/direct/super-call",
    "language/eval-code/direct/super-call-arrow",
    "language/eval-code/direct/super-call-fn",
    "language/eval-code/direct/super-call-method",
    "language/eval-code/direct/super-prop",
    "language/eval-code/direct/super-prop-arrow",
    "language/eval-code/direct/super-prop-dot-no-home",
    "language/eval-code/direct/super-prop-expr-no-home",
    "language/eval-code/direct/super-prop-expr-no-home-no-eval",
    "language/eval-code/direct/strict-caller-function-context",
    "language/eval-code/indirect/new.target",
    "language/eval-code/indirect/parse-failure-3",
    "language/eval-code/indirect/parse-failure-4",
    "language/eval-code/indirect/parse-failure-6",
    "language/eval-code/direct/parse-failure-6",
    "language/eval-code/indirect/super-call",
    "language/eval-code/indirect/super-prop",
    "language/expressions/call/eval-strictness-inherit-strict",
    "language/statements/break/S12.8_A7",
    "language/expressions/class/class-name-ident-await-escaped",
    "language/statements/class/class-name-ident-await-escaped",
    "language/statements/continue/S12.7_A7",
    "language/statements/function/13.0-8-s",
    "language/statements/function/13.1-2-s",
    "language/statements/function/13.1-4-s",
    "language/statements/labeled/value-await-non-module-escaped",
    "language/statements/try/catch-parameter-boundnames-restriction-arguments-eval-throws",
    "language/statements/try/catch-parameter-boundnames-restriction-eval-eval-throws",
    "language/statements/variable/12.2.1-18-s",
    "language/statements/variable/12.2.1-19-s",
    "language/statements/variable/12.2.1-2-s",
    "language/statements/variable/12.2.1-22-s",
    "language/statements/variable/12.2.1-3-s",
    "language/statements/variable/12.2.1-4-s",
    "language/statements/variable/12.2.1-7-s",
    "language/statements/variable/12.2.1-8-s",
    "language/statements/with/12.10.1-10-s",
    "language/statements/with/12.10.1-11-s",
    "language/statements/with/12.10.1-14-s",
    "language/statements/with/12.10.1-15-s",
    "language/statements/with/12.10.1-16-s",
    "language/statements/with/12.10.1-7-s",
    "built-ins/AsyncGeneratorFunction/instance-await-expr-in-param",
    "built-ins/AsyncGeneratorFunction/instance-yield-expr-in-param",
    "built-ins/GeneratorFunction/instance-yield-expr-in-param",
    "language/expressions/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/arrow-body-derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/arrow-body-derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/arrow-body-direct-eval-err-contains-arguments",
    "language/expressions/class/elements/arrow-body-indirect-eval-err-contains-newtarget",
    "language/expressions/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/arrow-body-private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/arrow-body-private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/arrow-body-private-direct-eval-err-contains-arguments",
    "language/expressions/class/elements/arrow-body-private-indirect-eval-err-contains-newtarget",
    "language/expressions/class/elements/derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/direct-eval-err-contains-arguments",
    "language/expressions/class/elements/indirect-eval-err-contains-newtarget",
    "language/expressions/class/elements/nested-derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/nested-derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/nested-derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/nested-derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/nested-derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/nested-direct-eval-err-contains-arguments",
    "language/expressions/class/elements/nested-indirect-eval-err-contains-newtarget",
    "language/expressions/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/nested-private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/nested-private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/nested-private-direct-eval-err-contains-arguments",
    "language/expressions/class/elements/nested-private-indirect-eval-err-contains-newtarget",
    "language/expressions/class/elements/private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/expressions/class/elements/private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/expressions/class/elements/private-derived-cls-direct-eval-err-contains-supercall",
    "language/expressions/class/elements/private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/expressions/class/elements/private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/expressions/class/elements/private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/expressions/class/elements/private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/expressions/class/elements/private-derived-cls-indirect-eval-err-contains-supercall",
    "language/expressions/class/elements/private-direct-eval-err-contains-arguments",
    "language/expressions/class/elements/private-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/arrow-body-derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/arrow-body-derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/arrow-body-derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/arrow-body-derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/arrow-body-direct-eval-err-contains-arguments",
    "language/statements/class/elements/arrow-body-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/arrow-body-private-derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/arrow-body-private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/arrow-body-private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/arrow-body-private-derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/arrow-body-private-direct-eval-err-contains-arguments",
    "language/statements/class/elements/arrow-body-private-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/direct-eval-err-contains-arguments",
    "language/statements/class/elements/indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/nested-derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/nested-derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/nested-derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/nested-derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/nested-derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/nested-derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/nested-direct-eval-err-contains-arguments",
    "language/statements/class/elements/nested-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/nested-private-derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/nested-private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/nested-private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/nested-private-derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/nested-private-direct-eval-err-contains-arguments",
    "language/statements/class/elements/nested-private-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/private-derived-cls-direct-eval-err-contains-supercall-1",
    "language/statements/class/elements/private-derived-cls-direct-eval-err-contains-supercall-2",
    "language/statements/class/elements/private-derived-cls-direct-eval-err-contains-supercall",
    "language/statements/class/elements/private-derived-cls-indirect-eval-contains-superproperty-1",
    "language/statements/class/elements/private-derived-cls-indirect-eval-contains-superproperty-2",
    "language/statements/class/elements/private-derived-cls-indirect-eval-err-contains-supercall-1",
    "language/statements/class/elements/private-derived-cls-indirect-eval-err-contains-supercall-2",
    "language/statements/class/elements/private-derived-cls-indirect-eval-err-contains-supercall",
    "language/statements/class/elements/private-direct-eval-err-contains-arguments",
    "language/statements/class/elements/private-indirect-eval-err-contains-newtarget",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-1",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-2",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-3",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-4",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-5",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-6",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-7",
    "language/statements/class/elements/privatename-not-valid-eval-earlyerr-8",
  )

  /** manually filtered out tests for in-progress features */
  lazy val manualInprogress = List(
    ("built-ins/String/prototype/matchAll/length", "matchAll"),
    ("built-ins/String/prototype/matchAll/name", "matchAll"),
    ("built-ins/String/prototype/matchAll/prop-desc", "matchAll"),
    ("built-ins/String/prototype/matchAll/regexp-is-null", "matchAll"),
    ("built-ins/String/prototype/matchAll/regexp-is-undefined", "matchAll"),
    (
      "built-ins/String/prototype/matchAll/regexp-matchAll-invocation",
      "matchAll",
    ),
    (
      "built-ins/String/prototype/matchAll/this-val-non-obj-coercible",
      "matchAll",
    ),
    (
      "built-ins/String/prototype/matchAll/regexp-matchAll-not-callable",
      "matchAll",
    ),
    (
      "built-ins/String/prototype/matchAll/regexp-prototype-has-no-matchAll",
      "matchAll",
    ),
    ("built-ins/RegExp/lookBehind/sliced-strings", "substr"),
  )

  /** manually filtered out non test files */
  lazy val manualNonTest = List(
    "built-ins/ShadowRealm/prototype/importValue/import-value_FIXTURE",
    "built-ins/ShadowRealm/prototype/importValue/import-value_syntax_error_FIXTURE",
    "built-ins/ShadowRealm/prototype/importValue/import-value_throws_FIXTURE",
  )

  /** manually filtered out long tests */
  lazy val longTest = List(
    "built-ins/Array/prototype/Symbol.unscopables/value",
    "built-ins/Array/prototype/concat/Array.prototype.concat_spreadable-sparse-object",
    "built-ins/Array/prototype/every/15.4.4.16-7-c-ii-2",
    "built-ins/Array/prototype/filter/15.4.4.20-9-c-ii-1",
    "built-ins/Array/prototype/flatMap/array-like-objects",
    "built-ins/Array/prototype/forEach/15.4.4.18-7-c-ii-1",
    "built-ins/Array/prototype/indexOf/15.4.4.14-10-1",
    "built-ins/Array/prototype/lastIndexOf/15.4.4.15-9-1",
    "built-ins/Array/prototype/map/15.4.4.19-8-c-ii-1",
    "built-ins/Array/prototype/some/15.4.4.17-7-c-ii-2",
    "built-ins/RegExp/S15.10.2.8_A3_T15",
    "built-ins/RegExp/S15.10.2.8_A3_T16",
    "built-ins/RegExp/property-escapes/generated/Changes_When_NFKC_Casefolded",
    "built-ins/RegExp/property-escapes/generated/General_Category_-_Letter",
    "built-ins/RegExp/property-escapes/generated/General_Category_-_Other",
    "built-ins/RegExp/property-escapes/generated/General_Category_-_Unassigned",
    "built-ins/RegExp/property-escapes/generated/ID_Continue",
    "built-ins/RegExp/property-escapes/generated/ID_Start",
    "language/expressions/call/tco-call-args",
    "language/expressions/call/tco-member-args",
    "language/expressions/class/async-gen-method-static/yield-star-async-throw",
    "language/expressions/comma/tco-final",
    "language/expressions/conditional/tco-cond",
    "language/expressions/conditional/tco-pos",
    "language/expressions/logical-and/tco-right",
    "language/expressions/logical-or/tco-right",
    "language/expressions/tco-pos",
    "language/reserved-words/ident-name-keyword-accessor",
    "language/reserved-words/ident-name-keyword-prop-name",
    "language/statements/block/tco-stmt",
    "language/statements/block/tco-stmt-list",
    "language/statements/do-while/tco-body",
    "language/statements/for/tco-const-body",
    "language/statements/for/tco-let-body",
    "language/statements/for/tco-lhs-body",
    "language/statements/for/tco-var-body",
    "language/statements/if/tco-else-body",
    "language/statements/if/tco-if-body",
    "language/statements/labeled/tco",
    "language/statements/return/tco",
    "language/statements/switch/tco-case-body",
    "language/statements/switch/tco-case-body-dflt",
    "language/statements/switch/tco-dftl-body",
    "language/statements/try/tco-catch",
    "language/statements/try/tco-catch-finally",
    "language/statements/try/tco-finally",
    "language/statements/while/tco-body",
    "language/expressions/coalesce/tco-pos-null",
    "language/expressions/coalesce/tco-pos-undefined",
    "language/types/number/8.5.1",
    "language/statements/function/S13.2.1_A1_T1",
  )

  /** manually filtered out very long tests */
  lazy val veryLongTest = List(
    "built-ins/Array/length/S15.4.5.2_A3_T4",
  )

  /** manually filtered out wrong test262 tests */
  lazy val wrongTest = List(
    "built-ins/AsyncGeneratorPrototype/return/return-state-completed-broken-promise",
    "built-ins/AsyncGeneratorPrototype/return/return-suspendedStart-broken-promise",
  )

  /** manually filtered out not yet supported tests */
  lazy val yets = List(
    "language/expressions/multiplication/S11.5.1_A4_T7", // math precision error
  )
}

/** helper of Test262 test filter */
object TestFilter:
  def fromDir(dirname: String): TestFilter = apply(MetaData.fromDir(dirname))
