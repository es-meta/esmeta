package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import esmeta.web.*

/** meta router */
object MetaRoute {
  def apply(): Route = concat(
    path("version") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            esmeta.VERSION.asJson.noSpaces,
          ),
        )
      }
    },
  )
}
