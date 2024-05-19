package esmeta.phase

import scala.util.*
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.SystemUtils.getFirstFilename
import esmeta.interpreter.Interpreter
import esmeta.util.SystemUtils.readFile
import esmeta.es.util.delta.DeltaDebugger
import esmeta.injector.Injector
import esmeta.js.JSEngine
import esmeta.js.minifier.Minifier
import esmeta.util.BaseUtils.error
import esmeta.injector.ReturnInjector

case object DeltaDebug extends Phase[CFG, String] {
  val name = "delta-debugger"

  val help = "delta-debugs ECMAScript program to minimize buggy program"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    val filename = getFirstFilename(cmdConfig, this.name)
    val code = readFile(filename)
    val finalState = Interpreter(cfg.init.from(code))
    val minified = Minifier.minifySwc(code) match
      case Failure(exception) => error(s"[delta-debug] $exception")
      case Success(minified)  => minified
    val injected =
      Injector
        .replaceBody(
          cfg,
          code,
          minified,
          defs = true,
          timeLimit = Some(1000),
          log = false,
          ignoreProperties = "\"name\"" :: Nil,
        )
        .toString
    val result = JSEngine.runGraal(injected, Some(1000)) match
      case Success(v) if v.isEmpty => error("[delta-debug] pass")
      case result @ _              => result.fold(_.toString, identity)
    DeltaDebugger(
      cfg,
      delta => {
        println(s"[delta-debug] trial\n${delta}")
        JSEngine.runGraal(delta, Some(1000)) match
          case Failure(_) => false
          case _ =>
            Minifier.minifySwc(delta) match
              case Failure(_) => false
              case Success(minifiedDelta) =>
                val injected = Injector
                  .replaceBody(
                    cfg,
                    code,
                    minifiedDelta,
                    defs = true,
                    timeLimit = Some(1000),
                    log = false,
                    ignoreProperties = "\"name\"" :: Nil,
                  )
                  .toString
                val test = JSEngine
                  .runGraal(injected, Some(1000))
                  .fold(_.toString, identity)
                  .toString
                println(s"injected: ${injected}")
                println(s"test: ${test}")
                if (test != result)
                  println(
                    s"expect:\n${result}\nactual:\n${test}",
                  )
                test == result

      },
      detail = true,
    ).result(code)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "checker",
      StrOption((c, v) => c.checker = v),
      "select checker",
    ),
  )
  case class Config(
    var checker: String = "minifier",
  )
}
