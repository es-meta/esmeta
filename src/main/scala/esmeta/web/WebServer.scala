package esmeta.web

import esmeta.cfg.CFG
import esmeta.web.routes.*
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.*
import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.*
import akka.http.scaladsl.server.Directives.*
import StatusCodes.*
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.*
import ch.megard.akka.http.cors.scaladsl.settings.*
import scala.io.StdIn

class WebServer(cfg: CFG, port: Int) {
  def run: Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "esmeta-web")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    // exception handler
    val exceptionHandler = ExceptionHandler {
      case e: Throwable =>
        e.printStackTrace
        complete(HttpResponse(InternalServerError, entity = e.toString))
    }

    // cors settings
    val settings = CorsSettings.defaultSettings
      .withAllowCredentials(false)
      .withMaxAge(None)
      .withAllowedMethods(List(GET, POST, PUT, DELETE))

    // root router
    val rootRoute = cors(settings) {
      handleExceptions(exceptionHandler)(
        concat(
          pathPrefix("spec")(SpecRoute(cfg)), // spec route
          pathPrefix("exec")(ExecRoute(cfg)), // exec route
          pathPrefix("breakpoint")(BreakpointRoute()), // breakpoint route
          pathPrefix("state")(StateRoute()), // state route
          pathPrefix("meta")(MetaRoute()), // meta route
        ),
      )
    }
    val bindingFuture = Http().newServerAt(ESMETA_HOST, port).bind(rootRoute)

    println(s"Server now online at http://$ESMETA_HOST:$port")
    println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
