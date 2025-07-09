package esmeta.web.http

import esmeta.cfg.CFG
import esmeta.web.http.routes.*
import zio.*
import zio.http.*
import zio.http.codec.PathCodec.literal

class WebServer(cfg: CFG, port: Int) {

  val serverConfig = Server.Config.default.binding(ESMETA_HOST, port)

  val allRoutes: Routes[Any, Response] =
    (literal("meta") / MetaRoute()) ++
    (literal("spec") / SpecRoute(cfg))

  val corsConfig = Middleware.CorsConfig(
    allowedOrigin = (_ => Some(Header.AccessControlAllowOrigin.All)),
    allowedMethods = Header.AccessControlAllowMethods(
      Method.GET,
      Method.POST,
      Method.PUT,
      Method.DELETE,
      Method.HEAD,
      Method.OPTIONS,
    ),
  )

  val routesWithMiddleware: Routes[Any, Response] =
    allRoutes @@ Middleware.cors(corsConfig)

  val serverLayer: TaskLayer[Server & Driver] =
    ZLayer.succeed(serverConfig) >>> Server.live.mapError(_ =>
      new RuntimeException("should never fail"),
    )

  val entry: ZIO[Any, Throwable, Unit] = for {
    serverFiber <- Server
      .serve(routesWithMiddleware)
      .provide(serverLayer)
      .fork
    addr = serverConfig.address
    _ <- Console.printLine(
      s"Starting server at http://${addr.getHostName()}:${addr.getPort()}",
    )
    _ <- Console.printLine("Press ENTER to stop the server")
    _ <- Console.readLine
    _ <- Console.printLine("Shutting down...")
    _ <- serverFiber.interrupt
  } yield ()

}
