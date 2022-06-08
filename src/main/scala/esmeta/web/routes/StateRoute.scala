package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import esmeta.web.*

/** state router */
object StateRoute {
  // root router
  def apply(): Route = concat(
    path("heap") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            debugger.heapInfo.asJson.noSpaces,
          ),
        )
      }
    },
    path("context" / IntNumber) { cid =>
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            debugger.ctxtInfo(cid).asJson.noSpaces,
          ),
        )
      }
    },
    path("callStack") {
      get {
        val info =
          try { debugger.callStackInfo.asJson.noSpaces }
          catch {
            case e: Throwable =>
              println(e)
              e.printStackTrace
              throw e
          }
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            debugger.callStackInfo.asJson.noSpaces,
          ),
        )
      }
    },
  )
}
