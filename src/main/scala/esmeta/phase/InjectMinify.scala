package esmeta.phase

import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.SystemUtils.getFirstFilename
import esmeta.util.SystemUtils.readFile
import esmeta.interpreter.Interpreter
import esmeta.injector.ReturnInjector
import esmeta.injector.Injector
import scala.util.*
import esmeta.js.JSEngine
import scala.collection.parallel.CollectionConverters._
import esmeta.js.minifier.Minifier
import esmeta.util.BoolOption

case object InjectMinify extends Phase[CFG, List[String]] {
  val name = "inject-minify"

  val help =
    "minifies and injects assertions to check final state of an ECMAScript code"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): List[String] =
    val filename = getFirstFilename(cmdConfig, this.name)
    val code = readFile(filename)
    val finalState = Interpreter(cfg.init.from(code))
    val returns = ReturnInjector(cfg, finalState).assertions
    (for (ret <- returns.par) yield {
      val wrapped = s"const k = (() => {\n$code\n$ret\n})();\n"
      Minifier.minifySwc(wrapped) match
        case Failure(exception) => println(exception); None
        case Success(minified) => {
          Some(
            Injector
              .replaceBody(
                cfg,
                wrapped,
                minified,
                defs = config.defs,
                timeLimit = Some(1000),
                log = false,
                ignoreProperties = "\"name\"" :: Nil,
              )
              .toString,
          )
        }
    }).flatten.toList

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "defs",
      BoolOption(c => c.defs = true),
      "prepend definitions of helpers for assertions.",
    ),
  )
  case class Config(
    var defs: Boolean = false,
  )
}
