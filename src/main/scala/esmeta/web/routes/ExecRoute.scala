package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import esmeta.cfg.CFG
import esmeta.state.DynamicAddr
import esmeta.web.*
import io.circe.*, io.circe.parser.*, io.circe.syntax.*

// exec router
object ExecRoute {

  /** helper function for steps with ignoreBreak flag */
  private def withDecodedBoolean(handler: Boolean => Debugger#StepResult) =
    entity(as[String]) { raw =>
      decode[Boolean](raw) match {
        case Left(err) => ??? // TODO handle error
        case Right(ignoreBreak) =>
          complete(
            handler(ignoreBreak).withAdditional().asHttpEntity,
          )
      }
    }

  // root router
  def apply(cfg: CFG): Route = {
    post {
      concat(
        path("run") {
          entity(as[String]) { raw =>
            decode[(String, List[(Boolean, Int, List[Int], Boolean)])](
              raw,
            ) match
              case Left(err) => ??? // TODO handle error
              case Right((sourceText, bpData)) =>
                initDebugger(cfg, sourceText)
                for { data <- bpData } debugger.addBreak(data)
                complete(
                  debugger.StepResult.ReachedFront.withAdditional().asHttpEntity,
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
            decode[(String, List[(Boolean, Int, List[Int], Boolean)], Int)](
              raw,
            ) match
              case Left(err) => ??? // TODO handle error
              case Right((sourceText, bpData, iterCount)) =>
                initDebugger(cfg, sourceText)
                for { data <- bpData } debugger.addBreak
                debugger.stepExactly(iterCount, true)
            complete(Json.Null.asHttpEntity)
          }
        },
        // spec step
        path("specStep") {
          withDecodedBoolean(debugger.specStep)
        },
        // spec step-over
        path("specStepOver") {
          withDecodedBoolean(debugger.specStepOver)
        },
        // spec step-out
        path("specStepOut") {
          withDecodedBoolean(debugger.specStepOut)
        },
        path("specStepBack") {
          withDecodedBoolean(debugger.specStepBack)
        },
        path("specStepBackOver") {
          withDecodedBoolean(debugger.specStepBackOver)
        },
        path("specStepBackOut") {
          withDecodedBoolean(debugger.specStepBackOut)
        },
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
        // IR-ES steps
        path("irStep") {
          withDecodedBoolean(debugger.irStep)
        },
        // spec step-over
        path("irStepOver") {
          withDecodedBoolean(debugger.irStepOver)
        },
        // spec step-out
        path("irStepOut") {
          withDecodedBoolean(debugger.irStepOut)
        },
        path("iterPlus") {
          withDecodedBoolean(debugger.iterPlus)
        },
        path("iterMinus") {
          withDecodedBoolean(debugger.iterMinus)
        },

        // ECMAScript ast steps
        path("esAstStep") {
          complete(
            debugger.esAstStep.withAdditional().asHttpEntity,
          )
        },
        // ECMAScript steps
        path("esStatementStep") {
          complete(
            debugger.esStatementStep.withAdditional().asHttpEntity,
          )
        },
        // ECMAScript step-over
        path("esStepOver") {
          complete(
            debugger.esStepOver.withAdditional().asHttpEntity,
          )
        },
        //  step-out
        path("esStepOut") {
          complete(
            debugger.esStepOut.withAdditional().asHttpEntity,
          )
        },
      )
    }
  }

  extension (stepResult: Debugger#StepResult) {
    def withAdditional(
      includeHeap: Boolean = false,
      includeReprint: Boolean = true,
    ): Json =
      Json.fromFields(
        List(
          "result" -> stepResult.ordinal.asJson,
          "callstack" -> debugger.callStackInfo.asJson,
          "stepCnt" -> debugger.getStepCnt.asJson,
          "iter" -> debugger.getIter.asJson,
        ) ++
        List(
          if (includeHeap) {
            Some("heap" -> debugger.heapInfo.asJson)
          } else None,
          if (includeReprint) {
            Some("reprint" -> debugger.st.sourceText.asJson)
          } else None,
        ).flatten,
      )
  }
}
