package esmeta.test262.util

import esmeta.*
import esmeta.test262.{*, given}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import java.io.*

/** test filter for Test262 */
object TestFilter {
  lazy val configSummary = getTests(languageFeatures)
    .remove(
      "longTest" -> (m => longTest.contains(removedExt(m.name))),
      "veryLongTest" -> (m => veryLongTest.contains(removedExt(m.name))),
      "TODO" -> (m => TODOs.contains(removedExt(m.name))),
    )
    .summary

  lazy val longConfigSummary = getTests(languageFeatures)
    .remove(
      "non longTest" -> (m => !longTest.contains(removedExt(m.name))),
    )
    .summary

  lazy val veryLongConfigSummary = getTests(languageFeatures)
    .remove(
      "non veryLongTest" -> (m => !veryLongTest.contains(removedExt(m.name))),
    )
    .summary

  // TODO
  // lazy val test262ManualconfigSummary =
  //   readJson[Test262ConfigSummary](s"$TEST_DIR/test262.json")

  /** all Test262 tests */
  lazy val allTests = walkTree(test262Dir).toList
    .filter(f => jsFilter(f.getName))
    .map(x => MetaData(x.toString))
    .sorted

  /** a getter of tests for given language features */
  def getTests(features: List[String]): List[MetaData] = allTests.remove(
    "harness" -> (_.name.startsWith("harness")),
    "internationalisation" -> (_.name.startsWith("intl")),
    "annex" -> (m =>
      m.name.startsWith("annex") ||
      m.name.contains("__proto__"),
    ),
    "in-progress features" -> (m =>
      !m.features.forall(features.contains(_)) ||
      manualInprogress.map(_._1).contains(removedExt(m.name)),
    ),
    "non-strict" -> (m =>
      m.flags.contains("noStrict") ||
      m.flags.contains("raw") ||
      manualNonstrict.contains(removedExt(m.name)),
    ),
    "module" -> (m =>
      m.flags.contains("module") ||
      m.name.startsWith("language/module-code/") ||
      m.name.startsWith("language/import/") ||
      m.name.startsWith("language/expressions/dynamic-import/") ||
      m.name.startsWith("language/expressions/import.meta/"),
    ),
    "early errors" -> (m =>
      !m.negative.isEmpty ||
      manualEarlyError.contains(removedExt(m.name)),
    ),
    "inessential built-in objects" -> (m =>
      m.flags.contains("CanBlockIsFalse") ||
      m.flags.contains("CanBlockIsTrue") ||
      !m.locales.isEmpty,
    ),
  )

  /** test262 directory */
  lazy val test262Dir = new File(s"$TEST_DIR/test262/test")

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
    "language/eval-code/indirect/new.target",
    "language/eval-code/indirect/parse-failure-3",
    "language/eval-code/indirect/parse-failure-4",
    "language/eval-code/indirect/super-call",
    "language/eval-code/indirect/super-prop",
    "language/expressions/call/eval-strictness-inherit-strict",
    "language/statements/break/S12.8_A7",
    "language/expressions/class/class-name-ident-await-escaped.js", // XXX check why not filtered
    "language/statements/class/class-name-ident-await-escaped.js", // XXX check why not filtered
    "language/statements/continue/S12.7_A7",
    "language/statements/function/13.0-8-s",
    "language/statements/function/13.1-2-s",
    "language/statements/function/13.1-4-s",
    "language/statements/labeled/value-await-non-module-escaped.js", // XXX check why not filtered
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
    "language/types/number/8.5.1",
  )

  /** manually filtered out very long tests */
  lazy val veryLongTest = List(
    "built-ins/Array/length/S15.4.5.2_A3_T4",
  )

  /** manually filtered out not yet supported tests */
  lazy val TODOs = List(
    "built-ins/Array/prototype/sort/stability-2048-elements",
    "built-ins/Array/prototype/toString/non-callable-join-string-tag",
    "built-ins/BigInt/asIntN/arithmetic",
    "built-ins/BigInt/asUintN/arithmetic",
    "built-ins/GeneratorPrototype/next/property-descriptor",
    "built-ins/NativeErrors/AggregateError/message-method-prop-cast",
    "built-ins/Number/bigint-conversion",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T1",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T2",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T3",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T4",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T5",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T6",
    "built-ins/RegExp/prototype/exec/S15.10.6.2_A3_T7",
    "built-ins/ShadowRealm/prototype/importValue/import-value_FIXTURE",
    "built-ins/ShadowRealm/prototype/importValue/import-value_syntax_error_FIXTURE",
    "built-ins/String/fromCodePoint/return-string-value",
    "built-ins/String/prototype/localeCompare/S15.5.4.9_A1_T1",
    "built-ins/String/prototype/localeCompare/S15.5.4.9_A1_T2",
    "built-ins/ThrowTypeError/name",
    "language/asi/S7.9_A3",
    "language/asi/S7.9_A9_T1",
    "language/asi/S7.9_A9_T2",
    "language/asi/S7.9_A9_T5",
    "language/asi/S7.9_A9_T9",
    "language/asi/do-while-same-line",
    "language/block-scope/leave/x-before-continue",
    "language/eval-code/direct/parse-failure-6",
    "language/eval-code/direct/strict-caller-function-context",
    "language/eval-code/indirect/parse-failure-6",
    "language/expressions/assignment/target-member-computed-reference-null",
    "language/expressions/assignment/target-member-computed-reference-undefined",
    "language/expressions/assignment/target-member-identifier-reference-null",
    "language/expressions/assignment/target-member-identifier-reference-undefined",
    "language/expressions/assignment/target-super-computed-reference-null",
    "language/expressions/assignment/target-super-identifier-reference-null",
    "language/expressions/async-generator/expression-yield-newline",
    "language/expressions/class/class-name-ident-await-escaped",
    "language/expressions/does-not-equals/bigint-and-boolean",
    "language/expressions/does-not-equals/bigint-and-number",
    "language/expressions/does-not-equals/bigint-and-number-extremes",
    "language/expressions/equals/bigint-and-boolean",
    "language/expressions/equals/bigint-and-number",
    "language/expressions/equals/bigint-and-number-extremes",
    "language/expressions/generators/yield-newline",
    "language/expressions/left-shift/bigint",
    "language/expressions/left-shift/bigint-non-primitive",
    "language/expressions/left-shift/bigint-toprimitive",
    "language/expressions/left-shift/bigint-wrapped-values",
    "language/expressions/object/method-definition/yield-newline",
    "language/expressions/optional-chaining/iteration-statement-do",
    "language/expressions/right-shift/bigint",
    "language/expressions/tagged-template/invalid-escape-sequences",
    "language/literals/regexp/7.8.5-1",
    "language/literals/regexp/S7.8.5_A1.3_T2",
    "language/literals/regexp/S7.8.5_A1.3_T4",
    "language/literals/regexp/S7.8.5_A1.5_T2",
    "language/literals/regexp/S7.8.5_A1.5_T4",
    "language/literals/regexp/S7.8.5_A2.3_T2",
    "language/literals/regexp/S7.8.5_A2.3_T4",
    "language/statements/async-generator/yield-star-return-missing-value-is-awaited",
    "language/statements/break/S12.8_A3",
    "language/statements/break/S12.8_A4_T1",
    "language/statements/break/S12.8_A4_T2",
    "language/statements/break/S12.8_A4_T3",
    "language/statements/break/line-terminators",
    "language/statements/class/class-name-ident-await-escaped",
    "language/statements/class/definition/methods-gen-yield-newline",
    "language/statements/do-while/S12.6.1_A1",
    "language/statements/do-while/S12.6.1_A10",
    "language/statements/do-while/S12.6.1_A11",
    "language/statements/do-while/S12.6.1_A14_T1",
    "language/statements/do-while/S12.6.1_A14_T2",
    "language/statements/do-while/S12.6.1_A2",
    "language/statements/do-while/S12.6.1_A4_T1",
    "language/statements/do-while/S12.6.1_A4_T2",
    "language/statements/do-while/S12.6.1_A4_T3",
    "language/statements/do-while/S12.6.1_A4_T4",
    "language/statements/do-while/S12.6.1_A4_T5",
    "language/statements/do-while/S12.6.1_A9",
    "language/statements/for-of/generator-close-via-continue",
    "language/statements/for-of/iterator-close-via-continue",
    "language/statements/function/S13.2.1_A1_T1",
    "language/statements/generators/yield-newline",
    "language/statements/labeled/value-await-non-module-escaped",
    "language/statements/return/line-terminators",
    "language/statements/try/S12.14_A9_T1",
    "language/statements/try/S12.14_A9_T2",
    "language/statements/try/S12.14_A9_T3",
    "language/statements/try/S12.14_A9_T4",
    "language/statements/try/S12.14_A9_T5",
    "language/statements/variable/S12.2_A12",
    "language/types/string/S8.4_A6.1",
    "language/types/string/S8.4_A6.2",
  )
}
