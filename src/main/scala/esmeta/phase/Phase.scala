package esmeta.phase

import esmeta.CommandConfig
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
    cmdConfig: CommandConfig,
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
  ): (Input, CommandConfig) => Output =
    val config = defaultConfig
    parser.addRule(config, name, options)
    (in, cmdConfig) => {
      if (!cmdConfig.silent)
        println(s"========================================")
        println(s" $name phase")
        println(s"----------------------------------------")
      apply(in, cmdConfig, config)
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
