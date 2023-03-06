package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.syntax.*
import esmeta.cfg.CFG
import esmeta.web.*

/** spec router */
object SpecRoute {

  /** root router */
  def apply(cfg: CFG): Route = concat(
    path("func") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            cfg.fnameMap
              .map { case (name, f) => (f.id, name) }
              .toList
              .asJson
              .noSpaces,
          ),
        )
      }
    },
  )
}
