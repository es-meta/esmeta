package esmeta.test262.util

import esmeta.*
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.test262.{*, given}
import io.circe.*, io.circe.syntax.*
import java.io.*

/** Test262 test filter */
case class TestFilter(spec: Spec, tests: List[MetaData]) {

  /** configuration summary for applicable tests */
  lazy val summary = targetTests
    .remove(
      "longTest" -> (m => longTest.contains(removedExt(m.relName))),
      "yet" -> (m => yets.contains(removedExt(m.relName))),
    )
    .summary

  /** configuration summary for long tests */
  lazy val longSummary = targetTests
    .remove(
      "non longTest" -> (m => !longTest.contains(removedExt(m.relName))),
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
      manualInprogress.contains(removedExt(m.relName)),
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

/** helper of Test262 test filter */
object TestFilter:
  def fromDir(spec: Spec, dirname: String): TestFilter =
    apply(spec, MetaData.fromDir(dirname))
