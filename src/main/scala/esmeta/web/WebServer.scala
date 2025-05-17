package esmeta.web

import esmeta.cfg.CFG
import esmeta.web.routes.*
import akka.Done
import akka.actor.CoordinatedShutdown
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.HttpMethods.*
import akka.http.scaladsl.server.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.settings.ServerSettings
import akka.pattern.after
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Sink, Source}
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.*
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
        complete(
          HttpResponse(StatusCodes.InternalServerError, entity = e.toString),
        )
    }

    // root router
    val rootRoute = cors() {
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

    // timeout for serial route
    val timeoutDuration = 30.seconds

    // serial queue with timeout
    val queue = Source
      .queue[(RequestContext, Promise[RouteResult])](
        1024,
        OverflowStrategy.backpressure,
      )
      .mapAsync(1) {
        case (ctx, promise) =>
          val routeFut = rootRoute(ctx)

          val timeoutFut = after(timeoutDuration) {
            Future.failed(new RuntimeException("Request timed out"))
          }

          val result = Future.firstCompletedOf(Seq(routeFut, timeoutFut))
          result.onComplete(promise.complete)
          result
      }
      .to(Sink.ignore)
      .run()

    // serialized route which handles request in fifo
    val serialRoute: Route = ctx => {
      val promise = Promise[RouteResult]()
      queue.offer((ctx, promise))
      promise.future
    }

    val bindingFuture = Http().newServerAt(ESMETA_HOST, port).bind(serialRoute)

    CoordinatedShutdown(system).addTask(
      CoordinatedShutdown.PhaseBeforeServiceUnbind,
      "log-shutdown-starting",
    ) { () =>
      println("coordinated shutdown triggered, port will be unbind...")
      Future.successful(Done)
    }

    println(s"Server now online at http://$ESMETA_HOST:$port")
    println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
