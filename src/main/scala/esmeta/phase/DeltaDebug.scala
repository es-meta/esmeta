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
    val originalCode = readFile(filename)
    val finalState = Interpreter(cfg.init.from(originalCode))
    val minifiedCode = Minifier.minifySwc(originalCode) match
      case Failure(exception) => error(s"[delta-debug] $exception")
      case Success(minified)  => minified
    val injected =
      Injector
        .replaceBody(
          cfg,
          originalCode,
          minifiedCode,
          defs = true,
          timeLimit = Some(1000),
          log = false,
          ignoreProperties = "\"name\"" :: Nil,
        )
    val testCode = injected.toString
    val assertions = injected.assertions
    val result = JSEngine.runGraal(testCode, Some(1000)) match
      case Success(v) if v.isEmpty => error("[delta-debug] pass")
      case result @ _              => result.fold(_.toString, identity)
    DeltaDebugger(
      cfg,
      delta => {
        println(s"[delta-debug] trial\n${delta}")
        // 1. check if it is valid program
        JSEngine.runGraal(delta, Some(1000)) match
          case Failure(_) => false
          case _          =>
            // 2. if it is valid, minify it
            Minifier.minifySwc(delta) match
              case Failure(_)             => false
              case Success(minifiedDelta) =>
                // 3. get assertions (final state) from the delta program
                val deltaAssertions = Injector
                  .getTest(
                    cfg,
                    delta,
                    defs = true,
                    log = false,
                    timeLimit = Some(1000),
                    ignoreProperties = "\"name\"" :: Nil,
                  )
                  .assertions
                // 4. if assertions between two program (original <-> delta), discard program.
                if (assertions != deltaAssertions)
                  println(s"= assertions\n$assertions\n")
                  println(s"= dassertions\n$deltaAssertions\n")
                  false
                else {
                  // 5. get assertion-injected program from delta program
                  val code = Injector
                    .replaceBody(
                      cfg,
                      originalCode,
                      minifiedDelta,
                      defs = true,
                      timeLimit = Some(1000),
                      log = false,
                      ignoreProperties = "\"name\"" :: Nil,
                    )
                    .toString
                  val test = JSEngine
                    .runGraal(code, Some(1000))
                    .fold(_.toString, identity)
                    .toString
                  // 6. run and check output from the injected code
                  if (test != result)
                    println(
                      s"expect:\n${result}\nactual:\n${test}",
                    )
                  test == result
                }
      },
      detail = true,
    ).result(originalCode)

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
