package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*,
io.circe.generic.semiauto.*
import esmeta.ir.*
import esmeta.web

/** breakpoint router */
object BreakpointRoute {
  // root router
  def apply(): Route = pathEnd {
    concat(
      // TODO add breakpoint
      post {
        entity(???) { bp =>
          complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
      // TODO delete breakpoint
      delete {
        entity(as[String]) { opt =>
          complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
      // TODO toggle breakpoint
      put {
        entity(as[String]) { opt =>
          complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
    )
  }
}
