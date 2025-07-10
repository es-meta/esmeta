package esmeta.web.http.route

import esmeta.cfg.CFG
import esmeta.spec.util.JsonProtocol.given
import esmeta.web.*
import esmeta.web.http.*
import esmeta.web.util.JsonProtocol
import cats.effect.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*

object SpecRoute {
  def apply(cfg: CFG) = HttpRoutes.of[IO] {
    case GET -> Root / "func" =>
      cfg.asJsonOk(using JsonProtocol(cfg).cfgToFuncEncoder)
    case GET -> Root / "version" =>
      cfg.spec.version.asJsonOk
  }
}
