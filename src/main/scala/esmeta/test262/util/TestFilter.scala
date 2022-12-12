package esmeta.test262.util

import esmeta.*
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.error.NotSupported.*
import esmeta.test262.{*, given}
import io.circe.*, io.circe.syntax.*
import java.io.*

/** Test262 test filter */
case class TestFilter(spec: Spec) {

  /** target tests and removed tests by each reason */
  def apply(
    tests: List[Test],
    withYet: Boolean = false,
    features: List[String] = languageFeatures,
  ): (List[Test], Iterable[(Test, ReasonPath)]) =
    var removedMap: Vector[(Test, ReasonPath)] = Vector()
    val targetTests = getFilters(withYet, features.toSet).foldLeft(tests) {
      case (tests, filter) =>
        for {
          test <- tests
          if filter(test) match
            case Some(reasonPath) => removedMap +:= test -> reasonPath; false
            case None             => true
        } yield test
    }
    (targetTests, removedMap)

  private def getFilters(
    withYet: Boolean,
    features: Set[String],
  ): List[Test => Option[ReasonPath]] = List(
    "harness" -> ((test: Test) => test.relName.startsWith("harness")),
    "internationalisation" -> ((test: Test) => test.relName.startsWith("intl")),
    "annex" -> ((test: Test) =>
      test.relName.startsWith("annex") ||
      test.relName.contains("__proto__"),
    ),
    "not-supported-features" -> ((test: Test) =>
      test.features.find(!features.contains(_)),
    ),
    "non-strict" -> ((test: Test) =>
      test.flags.contains("noStrict") ||
      test.flags.contains("raw"),
    ),
    "module" -> ((test: Test) =>
      test.flags.contains("module") ||
      test.relName.startsWith("language/module-code/") ||
      test.relName.startsWith("language/import/") ||
      test.relName.startsWith("language/expressions/dynamic-import/") ||
      test.relName.startsWith("language/expressions/import.meta/"),
    ),
    "negative-errors" -> ((test: Test) => test.negative.isDefined),
    "inessential-builtin-objects" -> ((test: Test) =>
      test.flags.contains("CanBlockIsFalse") ||
      test.flags.contains("CanBlockIsTrue") ||
      !test.locales.isEmpty,
    ),
    // manually filter tests
    test => manualFilterMap.get(removedExt(test.relName)),
    // manually filter not yet categorized tests
    "not-yet-categorized" -> ((test: Test) =>
      !withYet && manualYetCategorized.contains(removedExt(test.relName)),
    ),
  )

  given liftBool: Conversion[
    (String, Test => Boolean),
    Test => Option[ReasonPath],
  ] = pair =>
    val (name, filter) = pair
    val lifted = (x: Test) => if (filter(x)) Some(List(name)) else None
    lifted

  given liftReason: Conversion[
    (String, Test => Option[Reason]),
    Test => Option[ReasonPath],
  ] = pair =>
    val (name, filter) = pair
    test => filter(test).map(List(name, _))

  lazy val manualConfig = spec.manualInfo.test262

  lazy val manualFilterMap = (for {
    (reason, tests) <- manualConfig.filtered
    test <- tests
  } yield test -> List(reason)).toMap

  lazy val manualYetCategorized = manualConfig.yetCategorized.toSet

  /** language features in Test262
    * @url
    *   https://github.com/tc39/test262/blob/main/features.txt
    */
  lazy val languageFeatures = manualConfig.supportedFeatures
}
