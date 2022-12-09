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
    val filters = getFilters(withYet, features.toSet)
    var removedMap: Map[ReasonPath, List[Test]] = Map()
    val targetTests = filters.foldLeft(tests) {
      case (tests, (desc, filter)) =>
        val result = tests.groupBy(filter)
        removedMap ++=
          (for { (Some(path), tests) <- result } yield path -> tests)
        val remained = result.getOrElse(None, Nil)
        val removedCount = tests.length - remained.length
        if (removedCount > 0)
          println(f"- $desc%-30s: $removedCount%,5d tests are removed")
        remained
    }
    (targetTests, removedMap.toMap)
  }

  private def getFilters(
    withYet: Boolean,
    features: Set[String],
  ): List[(String, Test => Option[ReasonPath])] = List(
    lift("harness" -> (_.relName.startsWith("harness"))),
    lift("internationalisation" -> (_.relName.startsWith("intl"))),
    lift(
      "annex" -> (test =>
        test.relName.startsWith("annex") ||
        test.relName.contains("__proto__"),
      ),
    ),
    liftReason(
      "not-supported-features" -> (_.features.find(!features.contains(_))),
    ),
    lift(
      "non-strict" -> (test =>
        test.flags.contains("noStrict") ||
        test.flags.contains("raw") ||
        manualNonstrict.contains(removedExt(test.relName)),
      ),
    ),
    lift(
      "module" -> (test =>
        test.flags.contains("module") ||
        test.relName.startsWith("language/module-code/") ||
        test.relName.startsWith("language/import/") ||
        test.relName.startsWith("language/expressions/dynamic-import/") ||
        test.relName.startsWith("language/expressions/import.meta/"),
      ),
    ),
    lift(
      "negative-errors" -> (test =>
        test.negative.isDefined ||
        manualEarlyError.contains(removedExt(test.relName)),
      ),
    ),
    lift(
      "inessential-builtin-objects" -> (test =>
        test.flags.contains("CanBlockIsFalse") ||
        test.flags.contains("CanBlockIsTrue") ||
        !test.locales.isEmpty,
      ),
    ),
    lift(
      "non-tests" -> (test => manualNonTest.contains(removedExt(test.relName))),
    ),
    lift(
      "wrong-tests" -> (test => wrongTest.contains(removedExt(test.relName))),
    ),
    lift("longTest" -> (test => longTest.contains(removedExt(test.relName)))),
    lift("yet" -> (test => !withYet && yets.contains(removedExt(test.relName)))),
  )

  private def lift(
    pair: (String, Test => Boolean),
  ): (String, Test => Option[ReasonPath]) =
    val (name, filter) = pair
    val lifted = (x: Test) => if (filter(x)) Some(List(name)) else None
    name -> lifted

  private def liftReason(
    pair: (String, Test => Option[Reason]),
  ): (String, Test => Option[ReasonPath]) =
    val (name, filter) = pair
    name -> (test => filter(test).map(List(name, _)))

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
