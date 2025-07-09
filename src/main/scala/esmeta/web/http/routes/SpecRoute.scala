package esmeta.web.http.routes

import esmeta.cfg.CFG
import esmeta.spec.util.JsonProtocol.given
import esmeta.web.*
import esmeta.web.http.*
import esmeta.web.util.JsonProtocol
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import zio.*
import zio.http.*

object SpecRoute {
  def apply(cfg: CFG) = Routes(
    Method.GET / "func" -> handler { (req: Request) =>
      Response.json(
        cfg.asJson(using JsonProtocol(cfg).cfgToFuncEncoder).noSpaces,
      )
    },
    Method.GET / "version" -> handler { (req: Request) =>
      Response.json(cfg.spec.version.asJson.noSpaces)
    },
  )
}
