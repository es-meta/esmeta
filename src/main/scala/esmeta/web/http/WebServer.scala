package esmeta.web.http

import esmeta.cfg.CFG
import esmeta.web.http.routes.*
import esmeta.web.services.DebuggerService
import esmeta.web.util.JsonProtocol
import zio.*, zio.http.*
import zio.http.codec.PathCodec.literal

class WebServer(cfg: CFG, port: Int) {

  given JsonProtocol = JsonProtocol(cfg)

  val serverConfig = Server.Config.default.binding(ESMETA_HOST, port)

  val allRoutes: Routes[DebuggerService, Response] =
    literal("meta") / MetaRoute() ++
    literal("spec") / SpecRoute(cfg) ++
    literal("exec") / ExecRoute() ++
    literal("breakpoint") / BreakpointRoute()

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

  val routesWithMiddleware: Routes[DebuggerService, Response] =
    allRoutes @@ Middleware.cors(corsConfig)

  val serverLayer: TaskLayer[Server & Driver] =
    ZLayer.succeed(serverConfig) >>> Server.live.mapError(_ =>
      new RuntimeException("should never fail"),
    )

  val appLayers: ULayer[DebuggerService] =
    ZLayer.succeed(cfg) >>> DebuggerService.layer

  val entry: ZIO[DebuggerService, Throwable, Unit] = for {
    serverFiber <- Server
      .serve(routesWithMiddleware)
      // .provide(serverLayer)
      .provide(serverLayer, appLayers)
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
