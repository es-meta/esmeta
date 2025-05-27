package esmeta.web.http.routes

import esmeta.cfg.CFG
import esmeta.spec.util.JsonProtocol.given
import esmeta.web.*
import esmeta.web.http.*
import esmeta.web.util.JsonProtocol

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*

/** spec router */
object SpecRoute {

  /** root router */
  def apply(cfg: CFG): Route = concat(
    path("func") {
      get {
        complete(
          cfg
            .asJson(using JsonProtocol(cfg).cfgToFuncEncoder)
            .asHttpEntity,
        )
      }
    },
    path("version") {
      get {
        complete(
          cfg.spec.version.asJson.asHttpEntity,
        )
      }
    },
  )
}
