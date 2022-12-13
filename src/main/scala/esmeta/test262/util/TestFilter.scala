package esmeta.test262.util

import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.error.NotSupported.*
import esmeta.error.NotSupported.Category.*
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
    // harness files
    Harness -> ((test: Test) => test.relName.startsWith("harness")),
    // tests for internationalisation
    Internationalisation -> ((test: Test) => test.relName.startsWith("intl")),
    // tests for appendices, including web browsers
    Annex -> ((test: Test) =>
      test.relName.startsWith("annex") ||
      test.relName.contains("__proto__"),
    ),
    // tests for negative result, including parsing errors
    Negative -> ((test: Test) => test.negative.isDefined),
    // tests for non-strict mode
    NonStrict -> ((test: Test) =>
      test.flags.contains("noStrict") ||
      test.flags.contains("raw"),
    ),
    // tests for modules
    Module -> ((test: Test) =>
      test.flags.contains("module") ||
      test.relName.startsWith("language/module-code/") ||
      test.relName.startsWith("language/import/") ||
      test.relName.startsWith("language/expressions/dynamic-import/") ||
      test.relName.startsWith("language/expressions/import.meta/"),
    ),
    // fixtures
    Fixture -> ((test: Test) => removedExt(test.relName).endsWith("_FIXTURE")),
    // tests for language features not supported in ESMeta
    Feature -> ((test: Test) => test.features.find(!features.contains(_))),
    // manually filtered tests not yet categorized based on failure reasons
    YetCategorized -> ((test: Test) =>
      !withYet && manualYetCategorized.contains(removedExt(test.relName)),
    ),
    // manually filtered tests categorized based on failure reasons
    test => manualFilterMap.get(removedExt(test.relName)),
  )

  given liftBool: Conversion[
    (Category, Test => Boolean),
    Test => Option[ReasonPath],
  ] = pair =>
    val (category, filter) = pair
    val lifted = (x: Test) => if (filter(x)) Some(List(category.name)) else None
    lifted

  given liftReason: Conversion[
    (Category, Test => Option[Reason]),
    Test => Option[ReasonPath],
  ] = pair =>
    val (category, filter) = pair
    test => filter(test).map(List(category.name, _))

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
