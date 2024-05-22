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
import esmeta.es.util.fuzzer.MinifyTestResult
import esmeta.injector.NormalTag
import esmeta.util.SystemUtils.*

case object DeltaDebug extends Phase[CFG, String] {
  val name = "delta-debugger"

  val help = "delta-debugs ECMAScript program to minimize buggy program"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    if (config.multiple) {
      var minimals: Set[String] = Set.empty
      for {
        path <- cmdConfig.targets
        file <- walkTree(path)
        filename = file.toString
        if jsFilter(filename)
      } minimals += run(cfg, readFile(filename))
      println(s"minimal size: ${minimals.size}")
      println(minimals.mkString("\n"))
      ""
    } else {
      val filename = getFirstFilename(cmdConfig, this.name)
      val originalCode = readFile(filename)
      minifyTest(cfg, originalCode) match
        case None => println(s"[delta-debug] pass"); originalCode
        case Some(MinifyTestResult(original, minified, injected, exception)) =>
          DeltaDebugger(cfg, minifyTest(cfg, _).isDefined, detail = true)
            .result(original)
    }

  private def run(cfg: CFG, code: String): String =
    minifyTest(cfg, code) match
      case None => println(s"[delta-debug] pass"); code
      case Some(MinifyTestResult(original, minified, injected, exception)) =>
        DeltaDebugger(cfg, minifyTest(cfg, _).isDefined, detail = true)
          .result(original)

  private def minifyTest(cfg: CFG, code: String): Option[MinifyTestResult] =
    Minifier.minifySwc(code) match
      case Failure(exception) => println(s"[minify-test] $exception"); None
      case Success(minified) => {
        val injected =
          Injector.replaceBody(
            cfg,
            code,
            minified,
            defs = true,
            timeLimit = Some(1000),
            ignoreProperties = "\"name\"" :: Nil,
          )
        println(s"[minify-test] test start")
        println(s"[minify-test/code]\n$code")
        println(s"[minify-test/minified]\n$minified")
        injected.exitTag match
          case NormalTag =>
            val injectedCode = injected.toString
            JSEngine.runGraal(injectedCode, Some(1000)) match
              // minified program passes assertions
              case Success(v) if v.isEmpty =>
                println(s"[minify-test] pass"); None
              // minified program fails on assertions
              case Success(v) =>
                println(s"[minify-test] return value exists")
                Some(
                  MinifyTestResult(
                    code,
                    minified,
                    injectedCode,
                    v,
                  ),
                )
              // minified program throws exception
              // TODO(@hyp3rflow): we have to mask span data of program exception
              case Failure(exception) =>
                println(s"[minify-test] exception throws")
                Some(
                  MinifyTestResult(
                    code,
                    minified,
                    injectedCode,
                    exception.toString,
                  ),
                )

          case _ =>
            println(s"[minify-test] exit state is not normal"); None
      }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "checker",
      StrOption((c, v) => c.checker = v),
      "select checker",
    ),
    (
      "multiple",
      BoolOption(c => c.multiple = true),
      "delta-debug multiple programs",
    ),
  )
  case class Config(
    var checker: String = "minifier",
    var multiple: Boolean = false,
  )
}
