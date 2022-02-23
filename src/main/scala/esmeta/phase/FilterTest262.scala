package esmeta.phase

import esmeta.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.SystemUtils.*

/** `filter-test262` phase */
case object FilterTest262 extends Phase[Unit, ConfigSummary] {
  val name = "filter-test262"
  val help = "extracts and filters out metadata of Test262 tests."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): ConfigSummary = {
    if (DEBUG) println(s"[DEBUG] total ${TestFilter.allTests.length} tests.")
    TestFilter.configSummary
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config(
    var load: Option[String] = None,
  )
}
