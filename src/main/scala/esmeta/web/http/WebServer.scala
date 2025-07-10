package esmeta.web.http

import esmeta.cfg.CFG
import esmeta.web.http.route.*
import esmeta.web.service.*
import esmeta.web.util.JsonProtocol
import cats.effect.*
import com.comcast.ip4s.{Host, Port}
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.server.middleware.CORS

class WebServer(cfg: CFG, hostStr: String, portInt: Int) extends IOApp.Simple {
  override def run: IO[Unit] = for {
    service <- DebuggerService.of(cfg)

    given JsonProtocol = JsonProtocol(cfg)

    allRoutes: HttpRoutes[IO] = Router(
      "/meta" -> MetaRoute(),
      "/spec" -> SpecRoute(cfg),
      "/exec" -> ExecRoute(service).routes,
      "/breakpoint" -> BreakpointRoute(service).routes,
    )

    httpApp: HttpApp[IO] = CORS.policy.withAllowOriginAll(allRoutes).orNotFound

    host <- IO.fromOption(Host.fromString(hostStr))(
      new IllegalArgumentException(s"Invalid host: $hostStr"),
    )
    port <- IO.fromOption(Port.fromInt(portInt))(
      new IllegalArgumentException(s"Invalid port: $portInt"),
    )

    _ <- EmberServerBuilder
      .default[IO]
      .withHost(host)
      .withPort(port)
      .withHttpApp(httpApp)
      .build
      .use { server =>
        for {
          _ <- IO.println(
            s"Server started at ${server.address}. Press ENTER to shut down.",
          )
          _ <- IO.readLine
          _ <- IO.println("Shutting down...")
        } yield ()
      }
  } yield ()
}
