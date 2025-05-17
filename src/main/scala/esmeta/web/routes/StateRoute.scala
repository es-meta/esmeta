package esmeta.web.routes

import esmeta.web.*
import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*

/** state router */
object StateRoute {
  // root router
  def apply(): Route = concat(
    path("heap") {
      get {
        complete(
          debugger.heapInfo.asJson.asHttpEntity,
        )
      }
    },
  )
}
