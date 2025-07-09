package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.web.http.WebServer
import zio.*
import zio.http.*

/** `web` phase */
case object Web extends Phase[CFG, Unit] {
  val name = "web"
  val help = "starts a web server for an ECMAScript double debugger."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    val webServer = WebServer(cfg, config.port)
    val effectWithDependencies = ZIO.scoped(webServer.entry)
    val layersToProvide = webServer.serverLayer ++ webServer.appLayers
    val executableEffect = effectWithDependencies.provide(layersToProvide)

    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(executableEffect)
    }
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "port",
      NumOption((c, i) => c.port = i),
      "web server port (default: 8080).",
    ),
  )
  case class Config(var port: Int = 8080)
}
