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
  ): (List[Test], Map[ReasonPath, List[Test]]) = {
    val filters = getFilters(withYet, features)
    var removedMap: Map[ReasonPath, List[Test]] = Map()
    val targetTests = filters.foldLeft(tests) {
      case (tests, (desc, filter)) =>
        val result = tests.groupBy(filter)
        val removed = result.getOrElse(true, Nil)
        val removedCount = removed.length
        if (removedCount > 0)
          println(f"- $desc%-30s: $removedCount%,5d tests are removed")
        if (desc == "in-progress-features") {
          for {
            test <- tests;
            feature <- test.features
            if !features.contains(feature)
          }
            removedMap += List(desc, feature) -> removed
        } else {
          removedMap += List(desc) -> removed
        }
        result.getOrElse(false, Nil)
    }
    (targetTests, removedMap.toMap)
  }

  private def getFilters(
    withYet: Boolean,
    features: List[String],
  ): List[(String, Test => Boolean)] = List(
    "harness" -> (_.relName.startsWith("harness")),
    "internationalisation" -> (_.relName.startsWith("intl")),
    "annex" -> (test =>
      test.relName.startsWith("annex") ||
      test.relName.contains("__proto__"),
    ),
    "in-progress-features" -> (test =>
      !test.features.forall(features.contains(_)) ||
      manualInprogress.contains(removedExt(test.relName)),
    ),
    "non-strict" -> (test =>
      test.flags.contains("noStrict") ||
      test.flags.contains("raw") ||
      manualNonstrict.contains(removedExt(test.relName)),
    ),
    "module" -> (test =>
      test.flags.contains("module") ||
      test.relName.startsWith("language/module-code/") ||
      test.relName.startsWith("language/import/") ||
      test.relName.startsWith("language/expressions/dynamic-import/") ||
      test.relName.startsWith("language/expressions/import.meta/"),
    ),
    "negative-errors" -> (test =>
      test.negative.isDefined ||
      manualEarlyError.contains(removedExt(test.relName)),
    ),
    "inessential-builtin-objects" -> (test =>
      test.flags.contains("CanBlockIsFalse") ||
      test.flags.contains("CanBlockIsTrue") ||
      !test.locales.isEmpty,
    ),
    "non-tests" -> (test => manualNonTest.contains(removedExt(test.relName))),
    "wrong-tests" -> (test => wrongTest.contains(removedExt(test.relName))),
    "longTest" -> (test => longTest.contains(removedExt(test.relName))),
    "yet" -> (test => !withYet && yets.contains(removedExt(test.relName))),
  )

  lazy val manualConfig = spec.manualInfo.test262

  /** language features in Test262
    * @url
    *   https://github.com/tc39/test262/blob/main/features.txt
    */
  lazy val languageFeatures = manualConfig.supportedFeatures

  /** manually filtered out non-strict mode tests */
  lazy val manualNonstrict =
    manualConfig.filtered.getOrElse("non-strict tests", Nil).toSet

  /** manually filtered out tests for EarlyErorr */
  lazy val manualEarlyError =
    manualConfig.filtered.getOrElse("early errors", Nil).toSet

  /** manually filtered out tests for in-progress features */
  lazy val manualInprogress = (for {
    (_, names) <- manualConfig.inProgress
    name <- names
  } yield name).toSet

  /** manually filtered out non test files */
  lazy val manualNonTest =
    manualConfig.filtered.getOrElse("non tests", Nil).toSet

  /** manually filtered out long tests */
  lazy val longTest =
    manualConfig.filtered.getOrElse("long tests", Nil).toSet

  /** manually filtered out wrong test262 tests */
  lazy val wrongTest =
    manualConfig.filtered.getOrElse("wrong tests", Nil).toSet

  /** manually filtered out not yet supported tests */
  lazy val yets =
    manualConfig.filtered.getOrElse("yet tests", Nil).toSet
}
