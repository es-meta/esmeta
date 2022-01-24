package esmeta.phase

import esmeta.{GlobalConfig, SILENT}
import esmeta.util.ArgParser

/** phases
  *
  * @tparam Input
  *   phase input type
  * @tparam Output
  *   phase output type
  */
trait Phase[Input, Output] {

  /** phase name */
  val name: String

  /** help message */
  val help: String

  /** run phase */
  def apply(
    in: Input,
    globalConfig: GlobalConfig,
    config: Config = defaultConfig,
  ): Output

  /** default configuration */
  def defaultConfig: Config

  /** phase options */
  val options: List[PhaseOption[Config]]

  /** phase configuration type */
  type Config

  /** get phase runner */
  def getRunner(
    parser: ArgParser,
  ): (Input, GlobalConfig) => Output =
    val config = defaultConfig
    parser.addRule(config, name, options)
    (in, globalConfig) => {
      if (!SILENT)
        println(s"========================================")
        println(s" $name phase")
        println(s"----------------------------------------")
      apply(in, globalConfig, config)
    }

  /** get shape string of options */
  def getOptShapes: List[String] = options.map {
    case (opt, kind, _) =>
      s"-$name:${opt}${kind.postfix}"
  }

  /** get description strings of options */
  def getOptDescs: List[(String, String)] = options.map {
    case (opt, kind, desc) => (s"-$name:${opt}${kind.postfix}", desc)
  }
}
