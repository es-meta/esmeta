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
    path("iter") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            _debugger match
              case None        => ("no debugger").toString.asJson.noSpaces
              case Some(value) => value.getIter.asJson.noSpaces,
          ),
        )
      }
    },
    path("cursor") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            _debugger match
              case None => ("no debugger").toString.asJson.noSpaces
              case Some(value) =>
                value.st.context.cursor.toString.asJson.noSpaces,
          ),
        )
      }
    },
  )
}
