package esmeta.web.http.routes

import esmeta.cfg.CFG
import esmeta.es.Ast
import esmeta.state.DynamicAddr
import esmeta.web.*
import esmeta.web.http.*
import esmeta.web.util.JsonProtocol

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.parser.*, io.circe.syntax.*

// exec router
object ExecRoute {
  // root router
  def apply(cfg: CFG): Route = {
    val webJsonProtocol = JsonProtocol(cfg)
    import webJsonProtocol.given

    /** helper function for steps with ignoreBreak flag */
    def withStepOptions(
      handler: Debugger.StepOptions => Debugger#StepResult,
    ) = {
      entity(as[String]) { raw =>
        decode[Boolean](raw) match {
          case Left(err) => ??? // TODO handle error
          case Right(ignoreBreak) =>
            complete(
              handler(Debugger.StepOptions(ignoreBreak))
                .withAdditional()
                .asHttpEntity,
            )
        }
      }
    }

    post {
      concat(
        path("run") {
          entity(as[String]) { raw =>
            decode[(String, List[(Boolean, String, List[Int], Boolean)])](
              raw,
            ) match
              case Left(err) => ??? // TODO handle error
              case Right((sourceText, bpData)) =>
                initDebugger(cfg, sourceText)
                for { data <- bpData } debugger.addBreak(data)
                complete(
                  debugger.StepResult.ReachedFront
                    .withAdditional(
                      reprint = true,
                    )
                    .asHttpEntity,
                )
          }
        },
        // step back to provenance
        path("backToProvenance") {
          entity(as[String]) { raw =>
            decode[String](
              raw,
            ) match
              case Left(err)   => ??? // TODO handle error
              case Right(addr) =>
                // ToDo - support named address
                // ToDo - handle NumberFormatException
                complete(
                  debugger
                    .stepBackToProvenance(
                      DynamicAddr(addr.filter(_.isDigit).toLong),
                    )
                    .withAdditional()
                    .asHttpEntity,
                )
          }
        },
        // resume from iter count
        path("resumeFromIter") {
          entity(as[String]) { raw =>
            decode[(String, List[(Boolean, String, List[Int], Boolean)], Int)](
              raw,
            ) match
              case Left(err) => ??? // TODO handle error
              case Right((sourceText, bpData, iterCount)) =>
                initDebugger(cfg, sourceText)
                for { data <- bpData } debugger.addBreak(data)
                complete(
                  debugger
                    .stepExactly(iterCount, true)
                    .withAdditional(reprint = true)
                    .asHttpEntity,
                )
          }
        },

        // spec step
        path("specStep")(withStepOptions(debugger.specStep)),
        // spec step-over
        path("specStepOver")(withStepOptions(debugger.specStepOver)),
        // spec step-out
        path("specStepOut")(withStepOptions(debugger.specStepOut)),
        path("specStepBack")(withStepOptions(debugger.specStepBack)),
        path("specStepBackOver")(withStepOptions(debugger.specStepBackOver)),
        path("specStepBackOut")(withStepOptions(debugger.specStepBackOut)),
        // spec continue
        path("specContinue") {
          complete(
            debugger.continue.withAdditional().asHttpEntity,
          )
        },
        path("specRewind") {
          complete(
            debugger.rewind.withAdditional().asHttpEntity,
          )
        },
        path("irStep")(withStepOptions(debugger.irStep)),
        path("irStepOver")(withStepOptions(debugger.irStepOver)),
        path("irStepOut")(withStepOptions(debugger.irStepOut)),
        path("stepCntPlus")(withStepOptions(debugger.stepCntPlus)),
        path("stepCntMinus")(withStepOptions(debugger.stepCntMinus)),
        path("instCntPlus")(withStepOptions(debugger.instCntPlus)),
        path("instCntMinus")(withStepOptions(debugger.instCntMinus)),

        // ECMAScript-level steps
        path("esAstStep")(withStepOptions(debugger.esAstStep)),
        path("esStatementStep")(withStepOptions(debugger.esStatementStep)),
        path("esStepOver")(withStepOptions(debugger.esStepOver)),
        path("esStepOut")(withStepOptions(debugger.esStepOut)),
      )
    }
  }
  extension (stepResult: Debugger#StepResult) {
    def withAdditional(
      reprint: Boolean = false,
    )(using Encoder[Ast]): Json =
      Json.fromFields(
        List(
          "result" -> stepResult.ordinal.asJson,
          "callstack" -> debugger.callStackInfo.asJson,
          "stepCnt" -> debugger.getStepCnt.asJson,
          "instCnt" -> debugger.getInstCnt.asJson,
          "heap" -> debugger.heapInfo.asJson,
        ) ++ (if (reprint) then
                List(
                  "reprint" -> debugger.st.sourceText.asJson,
                  "ast" -> debugger.st.cachedAst.asJson,
                )
              else Nil),
      )
  }

}
