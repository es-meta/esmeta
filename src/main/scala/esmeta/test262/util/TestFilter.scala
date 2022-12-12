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
      case (tests, (desc, filter)) =>
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
  ): List[(String, Test => Option[ReasonPath])] =
    manualConfig.filtered.toList.map { (desc, tests) =>
      lift(
        desc -> (test =>
          if (desc == "yet")
            !withYet && tests.contains(removedExt(test.relName))
          else tests.contains(removedExt(test.relName)),
        ),
      )
    } ++ List(
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
          test.flags.contains("raw"),
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
        "negative-errors" -> (test => test.negative.isDefined),
      ),
      lift(
        "inessential-builtin-objects" -> (test =>
          test.flags.contains("CanBlockIsFalse") ||
          test.flags.contains("CanBlockIsTrue") ||
          !test.locales.isEmpty,
        ),
      ),
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

}
