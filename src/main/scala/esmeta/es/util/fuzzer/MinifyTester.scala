package esmeta.es.util.fuzzer

import scala.util.*
import esmeta.cfg.CFG
import esmeta.es.util.fuzzer.MinifyTesterResult
import esmeta.js.minifier.Minifier
import esmeta.injector.Injector
import esmeta.injector.NormalTag
import esmeta.js.JSEngine
import esmeta.es.util.delta.DeltaDebugger

case class MinifyTesterConfig(
  timeLimit: Option[Int] = Some(1000),
  ignoreProperties: List[String] = Nil,
  debugLevel: Int = 0,
)

class MinifyTester(
  cfg: CFG,
  config: MinifyTesterConfig = MinifyTesterConfig(),
) {
  val MinifyTesterConfig(timeLimit, ignoreProperties, debugLevel) = config

  def dd(code: String): String =
    test(code) match
      case None => log("minify-tester", "pass"); code
      case Some(MinifyTesterResult(original, minified, injected, exception)) =>
        DeltaDebugger(
          cfg, {
            test(_) match {
              case Some(result) => result.exception == exception
              case _            => false
            }
          },
          debugLevel > 1,
        )
          .result(original)

  def test(code: String): Option[MinifyTesterResult] =
    Minifier.minifySwc(code) match
      case Failure(exception) => log("minify-tester", s"$exception"); None
      case Success(minified) => {
        val injected = Try {
          Injector.replaceBody(
            cfg,
            code,
            minified,
            defs = true,
            timeLimit = timeLimit,
            ignoreProperties = ignoreProperties,
          )
        }
        log("minify-tester", s"test start")
        log("minify-tester/code", s"\n$code\n")
        log("minify-tester/minified", s"\n$minified\n")
        injected match
          case Success(i) if i.exitTag == NormalTag =>
            val injectedCode = i.toString
            JSEngine.runGraal(injectedCode, Some(1000)) match
              // minified program passes assertions
              case Success(v) if v.isEmpty =>
                log("minify-tester", "pass"); None
              // minified program fails on assertions
              case Success(v) =>
                log("minify-tester", "return value exists")
                Some(
                  MinifyTesterResult(
                    code,
                    minified,
                    injectedCode,
                    v,
                  ),
                )
              // minified program throws exception
              // TODO(@hyp3rflow): we have to mask span in exception description
              case Failure(exception) =>
                log(
                  "minify-tester",
                  s"exception throws\n${exception.toString}\n",
                )
                Some(
                  MinifyTesterResult(
                    code,
                    minified,
                    injectedCode,
                    exception.toString,
                  ),
                )
          case Success(_) =>
            log("minify-tester", "exit state is not normal"); None
          case Failure(exception) =>
            log("minify-tester", exception.toString); None
      }

  private def log(label: String, description: String) =
    if (debugLevel > 1) println(s"[$label] $description")

  private def info(label: String, description: String) =
    if (debugLevel > 0) System.err.println(s"[$label] $description")
}

case class MinifyTesterResult(
  originalCode: String,
  minifiedCode: String,
  injectedCode: String,
  exception: String,
)
