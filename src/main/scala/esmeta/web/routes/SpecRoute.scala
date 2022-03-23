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
  def apply(): Route = concat(
    path("func" / IntNumber) { fid =>
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            debugger.funcInfo(fid).asJson.noSpaces,
          ),
        )
      }
    },
  ),
}
