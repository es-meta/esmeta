package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*,
io.circe.generic.semiauto.*
import esmeta.web.*
import esmeta.cfg.CFG
import esmeta.interp.util.Debugger

// exec router
object ExecRoute {
  // TODO parameters for `/exec/run`
  // case class RunParams(breakpoints: List[Breakpoint], compressed: String)

  /** conversion for HTTP response */
  given Conversion[Debugger#StepResult, String] = _.ordinal.toString

  // root router
  def apply(cfg: CFG): Route = {
    post {
      concat(
        // TODO initialize debugger with breakpoints and JS source text
        path("run") {
          entity(as[String]) { sourceText =>
            initDebugger(cfg, sourceText)
            complete(HttpEntity(ContentTypes.`application/json`, "null"))
          }
        },
        // spec step
        path("specStep") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.specStep),
          )
        },
        // spec step-over
        path("specStepOver") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.specStepOver),
          )
        },
        // spec step-out
        path("specStepOut") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.specStepOut),
          )
        },
        // TODO js steps
        // path("jsStep") {
        //   complete(HttpEntity(ContentTypes.`application/json`, ""))
        // },
        // path("jsStepOver") {
        //   complete(HttpEntity(ContentTypes.`application/json`, ""))
        // },
        // path("jsStepOut") {
        //   complete(HttpEntity(ContentTypes.`application/json`, ""))
        // },
        // continue
        path("continue") {
          complete(
            HttpEntity(ContentTypes.`application/json`, debugger.continue),
          )
        },
      )
    }
  }
}
