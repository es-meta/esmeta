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

  /** conversion for HTTP response */
  given Conversion[Debugger#StepResult, String] = _.ordinal.toString

  /** helper function for steps with ignoreBreak flag */
  private def withDecodedBoolean(handler: Boolean => Debugger#StepResult) =
    entity(as[String]) { raw =>
      decode[Boolean](raw) match {
        case Left(err) => ??? // TODO handle error
        case Right(ignoreBreak) =>
          complete(
            HttpEntity(ContentTypes.`application/json`, handler(ignoreBreak)),
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
                val reprinted = debugger.st.sourceText match
                  case Some(value) => value.asJson.noSpaces
                  case None        => "null"
                complete(HttpEntity(ContentTypes.`application/json`, reprinted))
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
                  HttpEntity(
                    ContentTypes.`application/json`,
                    debugger.stepBackToProvenance(
                      DynamicAddr(addr.filter(_.isDigit).toLong),
                    ),
                  ),
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
            complete(HttpEntity(ContentTypes.`application/json`, "null"))
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
            HttpEntity(ContentTypes.`application/json`, debugger.continue),
          )
        },
        path("specRewind") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.rewind),
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
            HttpEntity(ContentTypes.`application/json`, debugger.esAstStep),
          )
        },
        // ECMAScript steps
        path("esStatementStep") {
          complete(
            HttpEntity(
              ContentTypes.`application/json`,
              debugger.esStatementStep,
            ),
          )
        },
        // ECMAScript step-over
        path("esStepOver") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.esStepOver),
          )
        },
        //  step-out
        path("esStepOut") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.esStepOut),
          )
        },
      )
    }
  }
}
