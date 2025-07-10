package esmeta.web.http

import esmeta.cfg.CFG
import esmeta.web.http.routes.*
import cats.effect.*
import com.comcast.ip4s.{Host, Port}
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.server.middleware.CORS

class WebServer(cfg: CFG, hostStr: String, portInt: Int) extends IOApp.Simple {

  val allRoutes: HttpRoutes[IO] = Router(
    "/meta" -> MetaRoute(),
    "/spec" -> SpecRoute(cfg),
    // TODO "/exec" -> ...
    // TODO "/breakpoint" -> ...
  )

  val httpApp: HttpApp[IO] = CORS.policy.withAllowOriginAll.apply(allRoutes).orNotFound

  private val host: Host = Host
    .fromString(hostStr)
    .getOrElse(throw new IllegalArgumentException(s"Invalid host: $hostStr"))

  private val port: Port = Port
    .fromInt(portInt)
    .getOrElse(throw new IllegalArgumentException(s"Invalid port: $portInt"))

  val serverResource: Resource[IO, Server] = EmberServerBuilder.default[IO].withHost(host).withPort(port).withHttpApp(httpApp).build

  override def run: IO[Unit] = {
    val program = serverResource.use { server =>
      for {
        _ <- IO.println(s"Server started at ${server.address}. Press ENTER to shut down.")
        _ <- IO.readLine
        _ <- IO.println("Shutting down...")
      } yield ()
    }

    program
  }
}