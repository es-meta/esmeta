package esmeta.phase

import scala.collection.mutable.{Set => MSet}
import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.es.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.es.util.mutator.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates a ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(cmdConfig, this.name)
    val ast = ESParser(cfg.spec.grammar)("Script").fromFile(filename)
    val mutator: Mutator = RandomMutation(cfg.grammar)
    val generator = SimpleAstGenerator(cfg.grammar)
    val synthesizer = RandomSynth(cfg.grammar)

    val test = false
    if (test) {
      generator.test()
      synthesizer.test()
    }

    var cur = ast
    var curStr = ""
    try {
      for (_ <- Range(0, 100)) {
        val ret = mutator.mutate(cur)
        val retStr = ret.toString(grammar = Some(cfg.grammar))
        curStr = cur.toString(grammar = Some(cfg.grammar))
        if (retStr.length > curStr.length) cur = ret
        println(curStr)

        if (
          ESParser(cfg.spec.grammar)("Script")
            .from(curStr)
            .toString(grammar = Some(cfg.grammar)) != curStr
        ) {
          throw new Exception()
        }
      }
    } catch {
      case _: Throwable =>
        println("parsing error"); println(curStr); println(cur)
    }
    curStr

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
